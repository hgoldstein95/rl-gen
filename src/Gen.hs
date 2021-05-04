{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Gen where

import Control.Lens (makeLenses, over, view, (^.))
import Control.Monad (ap, filterM, replicateM)
import Control.Monad.Fix (MonadFix (..))
import Data.Default (Default (..))
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Maybe as Maybe
import Data.Ord (comparing)
import System.Random (Random (..), RandomGen (..), StdGen, newStdGen)

type SelectId = String

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

newtype Gen s a = MkGen {unGen :: s -> QCGen -> Int -> (a, s)}

instance Functor (Gen s) where
  fmap f (MkGen h) =
    MkGen
      ( \s r n ->
          let (x, s') = h s r n
           in (f x, s')
      )

instance Applicative (Gen s) where
  pure x = MkGen (\s _ _ -> (x, s))
  (<*>) = ap
  _ *> m = m
  m <* _ = m

instance Monad (Gen s) where
  return = pure
  MkGen m >>= k =
    MkGen
      ( \s r n ->
          case split r of
            (r1, r2) ->
              let (x, s') = m s r1 n
                  MkGen m' = k x
               in m' s' r2 n
      )
  (>>) = (*>)

class Select s where
  select :: SelectId -> [s a] -> s a

instance Select (Gen s) where
  select _ = uniform

get :: Gen s s
get = MkGen $ \s _ _ -> (s, s)

put :: s -> Gen s ()
put s = MkGen $ \_ _ _ -> ((), s)

update :: (s -> s) -> Gen s ()
update f = MkGen $ \s _ _ -> ((), f s)

sized :: (Int -> Gen s a) -> Gen s a
sized f = MkGen (\s r n -> let MkGen m = f n in m s r n)

getSize :: Gen s Int
getSize = sized pure

resize :: Int -> Gen s a -> Gen s a
resize n _ | n < 0 = error "Test.QuickCheck.resize: negative size"
resize n (MkGen g) = MkGen (\s r _ -> g s r n)

scale :: (Int -> Int) -> Gen s a -> Gen s a
scale f g = sized (\n -> resize (f n) g)

choose :: Random a => (a, a) -> Gen s a
choose rng = MkGen (\s r _ -> let (x, _) = randomR rng r in (x, s))

chooseAny :: Random a => Gen s a
chooseAny = MkGen (\s r _ -> let (x, _) = random r in (x, s))

generate :: Default s => Gen s a -> IO a
generate (MkGen g) =
  do
    r <- newQCGen
    return (fst $ g def r 30)

sample' :: Default s => Gen s a -> IO [a]
sample' g =
  generate (sequence [resize n g | n <- [0, 2 .. 20]])

sample :: (Default s, Show a) => Gen s a -> IO ()
sample g =
  do
    cases <- sample' g
    mapM_ print cases

suchThat :: Gen s a -> (a -> Bool) -> Gen s a
gen `suchThat` p =
  do
    mx <- gen `suchThatMaybe` p
    case mx of
      Just x -> return x
      Nothing -> sized (\n -> resize (n + 1) (gen `suchThat` p))

suchThatMap :: Gen s a -> (a -> Maybe b) -> Gen s b
gen `suchThatMap` f =
  fmap fromJust $ fmap f gen `suchThat` isJust

suchThatMaybe :: Gen s a -> (a -> Bool) -> Gen s (Maybe a)
gen `suchThatMaybe` p = sized (\n -> try n (2 * n))
  where
    try m n
      | m > n = return Nothing
      | otherwise = do
        x <- resize m gen
        if p x then return (Just x) else try (m + 1) n

uniform :: [Gen s a] -> Gen s a
uniform [] = error "QuickCheck.uniform used with empty list"
uniform gs = choose (0, length gs - 1) >>= (gs !!)

frequency :: [(Int, Gen s a)] -> Gen s a
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

elements :: [a] -> Gen s a
elements [] = error "QuickCheck.elements used with empty list"
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

sublistOf :: [a] -> Gen s [a]
sublistOf = filterM (\_ -> choose (False, True))

shuffle :: [a] -> Gen s [a]
shuffle xs = do
  ns <- vectorOf (length xs) (choose (minBound :: Int, maxBound))
  return (map snd (sortBy (comparing fst) (zip ns xs)))

growingElements :: [a] -> Gen s a
growingElements [] = error "QuickCheck.growingElements used with empty list"
growingElements xs = sized $ \n -> elements (take (1 `max` size n) xs)
  where
    k = length xs
    mx = 100
    log' = round . log . toDouble
    size n = (log' n + 1) * k `div` log' mx
    toDouble = fromIntegral :: Int -> Double

listOf :: Gen s a -> Gen s [a]
listOf gen = sized $ \n ->
  do
    k <- choose (0, n)
    vectorOf k gen

listOf1 :: Gen s a -> Gen s [a]
listOf1 gen = sized $ \n ->
  do
    k <- choose (1, 1 `max` n)
    vectorOf k gen

vectorOf :: Int -> Gen s a -> Gen s [a]
vectorOf = replicateM

infiniteListOf :: Gen s a -> Gen s [a]
infiniteListOf gen = sequence (repeat gen)