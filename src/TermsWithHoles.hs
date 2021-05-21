{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module TermsWithHoles where

import BCP_5_14 (MonadAutoGen (..))
import Test.QuickCheck (Gen)

type Depth = Int

data BSTF a
  = Node' a Int a
  deriving (Show, Functor, Eq)

data Free f a
  = Pure a
  | Free (f (Free f a))

type BST = Free BSTF ()

type HBST = Free BSTF Bool

instance (Eq (f (Free f a)), Eq a) => Eq (Free f a) where
  Pure x == Pure y = x == y
  Free f == Free g = f == g
  _ == _ = False

instance (Show a, Show (f (Free f a))) => Show (Free f a) where
  show (Pure x) = show x
  show (Free f) = "(" ++ show f ++ ")"

pattern Node :: Free BSTF a -> Int -> Free BSTF a -> Free BSTF a
pattern Node l x r = Free (Node' l x r)

pattern Leaf :: BST
pattern Leaf = Pure ()

pattern HLeaf :: HBST
pattern HLeaf = Pure True

pattern Hole :: HBST
pattern Hole = Pure False

freeze :: HBST -> BST
freeze (Node l x r) = Node (freeze l) x (freeze r)
freeze HLeaf = Leaf
freeze Hole = error "cannot freeze term with hole"
freeze _ = undefined

genTree :: Gen BST
genTree = freeze <$> aux Hole
  where
    aux t = do
      t' <- growTree t
      if t == t' then pure t' else aux t'

growTree :: HBST -> Gen HBST
growTree HLeaf = pure HLeaf
growTree (Node l x r) = do
  l' <- growTree l
  r' <- if l' == l then growTree r else pure r
  pure $ Node l' x r'
growTree Hole =
  select
    "NODE_TYPE"
    [ pure HLeaf,
      select "NODE_VAL" (pure <$> [1 .. 100]) $ \x -> pure $ Node Hole x Hole
    ]
    pure
growTree _ = undefined