{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module BCP_5_14 where

import BST (BST (..))
import QuickCheck.GenT (MonadGen (..))
import Test.QuickCheck (Gen, generate, oneof)

data Learner a = Learner {learnerFn :: Gen (a, Double -> Learner a), rewardFn :: a -> Double}

class MonadGen g => MonadAutoGen g where
  select :: [g a] -> g a

instance MonadAutoGen Gen where
  select = oneof

-- GOAL: Keep this structure, but come up with a definition for g that has enough info for MCC
genTree :: MonadAutoGen g => g BST
genTree = sized aux
  where
    aux 0 = pure Leaf
    aux n =
      select
        [ pure Leaf,
          Node
            <$> aux (n `div` 2)
            <*> select (pure <$> [0 .. 10])
            <*> aux (n `div` 2)
        ]

asGen :: (forall g. MonadAutoGen g => g a) -> Learner a
asGen g =
  Learner
    ( do
        x <- g
        pure (x, const (asGen g))
    )
    (const 0)

asMCCGen :: (forall g. MonadAutoGen g => g a) -> (a -> Double) -> Learner a
asMCCGen _ _ = undefined -- TODO

generateN :: Int -> Learner a -> IO [a]
generateN 0 _ = pure []
generateN n (Learner l r) = do
  (x, f) <- generate l
  (x :) <$> generateN (n - 1) (f $ r x)
