{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module BCP_5_14 where

import BST (BST (..), isBST, size)
import Control.Monad.Reader (local)
import Control.Monad.State.Strict (put, runState)
import Control.Monad.Trans (lift)
import Data.Default (def)
import qualified Data.Set as Set
import MCCGen (MCCGen, mccreward, mccselect, (<:>))
import QuickCheck.GenT (MonadGen (..), runGenT)
import Test.QuickCheck (Gen, generate, oneof)

newtype Learner a = Learner {learnerFn :: Gen (a, Learner a)}

class MonadGen g => MonadAutoGen g where
  select :: String -> [g a] -> g a
  context :: String -> g a -> g a

instance MonadAutoGen Gen where
  select _ = oneof
  context _ g = g

instance MonadAutoGen MCCGen where
  select = mccselect
  context s = local (s <:>)

-- GOAL: Keep this structure, but come up with a definition for g that has enough info for MCC
genTree :: MonadAutoGen g => g BST
genTree = resize 4 $ sized aux
  where
    aux 0 = pure Leaf
    aux depth = do
      x <- select "NODE_VAL" (pure <$> [0 .. 10])
      context ("VAL(" ++ show x ++ ")") $
        select
          "NODE_TYPE"
          [ pure Leaf,
            Node
              <$> context "LEFT" (aux (depth - 1))
              <*> pure x
              <*> context "RIGHT" (aux (depth - 1))
          ]

learnGen :: Gen a -> Learner a
learnGen g = Learner $ do
  x <- g
  pure (x, learnGen g)

learnMCC :: Eq a => MCCGen a -> (a -> Double) -> Learner a
learnMCC gen = aux (pure ())
  where
    aux setup r = Learner $ do
      (x, s) <- (`runState` def) <$> runGenT (setup >> gen)
      pure
        ( x,
          aux
            (lift (put s) >> mccreward (r x))
            (\y -> if x == y then 0 else r y)
        )

generateN :: Int -> Learner a -> IO [a]
generateN 0 _ = pure []
generateN n l = do
  (x, l') <- generate $ learnerFn l
  (x :) <$> generateN (n - 1) l'

test :: Int -> IO ()
test total = do
  aux "RAND:" (generateN total $ learnGen genTree)
  aux "RL  :" (generateN total $ learnMCC genTree (\t -> if isBST t then 20 else -1))
  where
    aux n io = do
      putStr n
      ts <- filter isBST <$> io
      putStr $ " " ++ show (length ts) ++ " valid BSTs"
      putStr $ ", " ++ show (length . Set.fromList $ ts) ++ " unique valid BSTs"
      putStr $ ", " ++ show (maximum . map size $ ts) ++ " max size"
      putStrLn ""