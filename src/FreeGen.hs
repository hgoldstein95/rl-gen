{-# LANGUAGE GADTs #-}

module FreeGen where

import BST (BST (..))
import Control.Monad (ap)

data FGen a where
  Pure :: a -> FGen a
  Bind :: FGen a -> (a -> FGen b) -> FGen b
  Select :: [FGen a] -> FGen a

instance Functor FGen where
  fmap f (Pure a) = Pure (f a)
  fmap f (Bind ma g) = Bind ma (fmap f . g)
  fmap f (Select ms) = Select (map (fmap f) ms)

instance Applicative FGen where
  pure = Pure
  (<*>) = ap

instance Monad FGen where
  (>>=) = Bind

genTree :: FGen BST
genTree = do
  Select
    [ pure Leaf,
      Node
        <$> genTree
        <*> Select (pure <$> [1 .. 10])
        <*> genTree
    ]