{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module GenT where

import Control.Monad (ap, filterM, replicateM)
import Control.Monad.Trans (MonadTrans (..))
import Data.List (sortBy)
import Data.Maybe (fromJust, isJust)
import Data.Ord (comparing)
import System.Random (Random (..), RandomGen (..), StdGen, newStdGen)

newtype QCGen = QCGen StdGen

newQCGen :: IO QCGen
newQCGen = fmap QCGen newStdGen

instance RandomGen QCGen where
  split (QCGen g) =
    case split g of
      (g1, g2) -> (QCGen g1, QCGen g2)
  genRange (QCGen g) = genRange g
  next = wrapQCGen next
    where
      wrapQCGen f (QCGen g) =
        case f g of
          (x, g') -> (x, QCGen g')

newtype GenT m a = MkGenT {unGenT :: QCGen -> Int -> m a}

instance MonadTrans GenT where
  lift m = MkGenT (\_ _ -> m)

instance Monad m => Functor (GenT m) where
  fmap f (MkGenT h) =
    MkGenT
      ( \r n ->
          f <$> h r n
      )

instance Monad m => Applicative (GenT m) where
  pure x = MkGenT (\_ _ -> pure x)
  (<*>) = ap

instance Monad m => Monad (GenT m) where
  return = pure
  MkGenT m >>= k =
    MkGenT
      ( \r n ->
          case split r of
            (r1, r2) -> do
              MkGenT m' <- k <$> m r1 n
              m' r2 n
      )

sized :: (Int -> GenT m a) -> GenT m a
sized f = MkGenT (\r n -> let MkGenT m = f n in m r n)

resize :: Int -> GenT m a -> GenT m a
resize n _ | n < 0 = error "Test.QuickCheck.resize: negative size"
resize n (MkGenT g) = MkGenT (\r _ -> g r n)

choose :: (Random a, Monad m) => (a, a) -> GenT m a
choose rng = MkGenT (\r _ -> let (x, _) = randomR rng r in pure x)

chooseAny :: (Random a, Monad m) => GenT m a
chooseAny = MkGenT (\r _ -> let (x, _) = random r in pure x)

generate :: (m a -> a) -> GenT m a -> IO a
generate runM (MkGenT g) = do
  r <- newQCGen
  return (runM (g r 30))

getSize :: Monad m => GenT m Int
getSize = sized pure

scale :: (Int -> Int) -> GenT m a -> GenT m a
scale f g = sized (\n -> resize (f n) g)

-- sample' :: (m a -> a) -> GenT m a -> IO [a]
-- sample' runM g =
--   generate undefined (sequence [resize n g | n <- [0, 2 .. 20]])

-- sample :: Show a => GenT m a -> IO ()
-- sample g =
--   do
--     cases <- sample' g
--     mapM_ print cases

suchThat :: Monad m => GenT m a -> (a -> Bool) -> GenT m a
gen `suchThat` p =
  do
    mx <- gen `suchThatMaybe` p
    case mx of
      Just x -> return x
      Nothing -> sized (\n -> resize (n + 1) (gen `suchThat` p))

suchThatMap :: Monad m => GenT m a -> (a -> Maybe b) -> GenT m b
gen `suchThatMap` f =
  fmap fromJust $ fmap f gen `suchThat` isJust

suchThatMaybe :: Monad m => GenT m a -> (a -> Bool) -> GenT m (Maybe a)
gen `suchThatMaybe` p = sized (\n -> try n (2 * n))
  where
    try m n
      | m > n = return Nothing
      | otherwise = do
        x <- resize m gen
        if p x then return (Just x) else try (m + 1) n

uniform :: Monad m => [GenT m a] -> GenT m a
uniform [] = error "QuickCheck.uniform used with empty list"
uniform gs = choose (0, length gs - 1) >>= (gs !!)

frequency :: Monad m => [(Int, GenT m a)] -> GenT m a
frequency [] = error "QuickCheck.frequency used with empty list"
frequency xs
  | any ((< 0) . fst) xs =
    error "QuickCheck.frequency: negative weight"
  | all ((== 0) . fst) xs =
    error "QuickCheck.frequency: all weights were zero"
frequency xs0 = choose (1, tot) >>= (`pick` xs0)
  where
    tot = sum (map fst xs0)

    pick n ((k, x) : xs)
      | n <= k = x
      | otherwise = pick (n - k) xs
    pick _ _ = error "QuickCheck.pick used with empty list"

elements :: Monad m => [a] -> GenT m a
elements [] = error "QuickCheck.elements used with empty list"
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

sublistOf :: Monad m => [a] -> GenT m [a]
sublistOf = filterM (\_ -> choose (False, True))

shuffle :: Monad m => [a] -> GenT m [a]
shuffle xs = do
  ns <- vectorOf (length xs) (choose (minBound :: Int, maxBound))
  return (map snd (sortBy (comparing fst) (zip ns xs)))

growingElements :: Monad m => [a] -> GenT m a
growingElements [] = error "QuickCheck.growingElements used with empty list"
growingElements xs = sized $ \n -> elements (take (1 `max` size n) xs)
  where
    k = length xs
    mx = 100
    log' = round . log . toDouble
    size n = (log' n + 1) * k `div` log' mx
    toDouble = fromIntegral :: Int -> Double

listOf :: Monad m => GenT m a -> GenT m [a]
listOf gen = sized $ \n ->
  do
    k <- choose (0, n)
    vectorOf k gen

listOf1 :: Monad m => GenT m a -> GenT m [a]
listOf1 gen = sized $ \n ->
  do
    k <- choose (1, 1 `max` n)
    vectorOf k gen

vectorOf :: Monad m => Int -> GenT m a -> GenT m [a]
vectorOf = replicateM

infiniteListOf :: Monad m => GenT m a -> GenT m [a]
infiniteListOf gen = sequence (repeat gen)