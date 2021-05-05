{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module MonadGen where

import Control.Monad (filterM, replicateM)
import Data.List (sortBy)
import Data.Maybe (fromJust, isJust)
import Data.Ord (comparing)
import System.Random (Random)

class Monad g => MonadGen g where
  choose :: Random a => (a, a) -> g a
  chooseAny :: Random a => g a
  sized :: (Int -> g a) -> g a
  resize :: Int -> g a -> g a
  generate :: g a -> IO a

getSize :: MonadGen g => g Int
getSize = sized pure

scale :: MonadGen g => (Int -> Int) -> g a -> g a
scale f g = sized (\n -> resize (f n) g)

sample' :: MonadGen g => g a -> IO [a]
sample' g =
  generate (sequence [resize n g | n <- [0, 2 .. 20]])

sample :: (Show a, MonadGen g) => g a -> IO ()
sample g =
  do
    cases <- sample' g
    mapM_ print cases

suchThat :: MonadGen g => g a -> (a -> Bool) -> g a
gen `suchThat` p =
  do
    mx <- gen `suchThatMaybe` p
    case mx of
      Just x -> return x
      Nothing -> sized (\n -> resize (n + 1) (gen `suchThat` p))

suchThatMap :: MonadGen g => g a -> (a -> Maybe b) -> g b
gen `suchThatMap` f =
  fmap fromJust $ fmap f gen `suchThat` isJust

suchThatMaybe :: MonadGen g => g a -> (a -> Bool) -> g (Maybe a)
gen `suchThatMaybe` p = sized (\n -> try n (2 * n))
  where
    try m n
      | m > n = return Nothing
      | otherwise = do
        x <- resize m gen
        if p x then return (Just x) else try (m + 1) n

uniform :: MonadGen g => [g a] -> g a
uniform [] = error "QuickCheck.uniform used with empty list"
uniform gs = choose (0, length gs - 1) >>= (gs !!)

frequency :: MonadGen g => [(Int, g a)] -> g a
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

elements :: MonadGen g => [a] -> g a
elements [] = error "QuickCheck.elements used with empty list"
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

sublistOf :: MonadGen g => [a] -> g [a]
sublistOf = filterM (\_ -> choose (False, True))

shuffle :: MonadGen g => [a] -> g [a]
shuffle xs = do
  ns <- vectorOf (length xs) (choose (minBound :: Int, maxBound))
  return (map snd (sortBy (comparing fst) (zip ns xs)))

growingElements :: MonadGen g => [a] -> g a
growingElements [] = error "QuickCheck.growingElements used with empty list"
growingElements xs = sized $ \n -> elements (take (1 `max` size n) xs)
  where
    k = length xs
    mx = 100
    log' = round . log . toDouble
    size n = (log' n + 1) * k `div` log' mx
    toDouble = fromIntegral :: Int -> Double

listOf :: MonadGen g => g a -> g [a]
listOf gen = sized $ \n ->
  do
    k <- choose (0, n)
    vectorOf k gen

listOf1 :: MonadGen g => g a -> g [a]
listOf1 gen = sized $ \n ->
  do
    k <- choose (1, 1 `max` n)
    vectorOf k gen

vectorOf :: MonadGen g => Int -> g a -> g [a]
vectorOf = replicateM

infiniteListOf :: MonadGen g => g a -> g [a]
infiniteListOf gen = sequence (repeat gen)

type SelectId = String

class Select s where
  type Ctx s :: *
  select :: SelectId -> Ctx s -> [s a] -> s a