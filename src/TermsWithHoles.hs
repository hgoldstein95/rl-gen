{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module TermsWithHoles where

import BCP_5_14 (MonadAutoGen (..))
import QuickCheck.GenT (MonadGen (..))

type Depth = Int

data GBST
  = Leaf
  | Node GBST Int GBST
  | Hole
  deriving (Eq)

instance Show GBST where
  show Leaf = "L"
  show (Node l x r) = "(" ++ show l ++ " " ++ show x ++ " " ++ show r ++ ")"
  show Hole = "_"

size :: GBST -> Int
size Leaf = 0
size (Node l _ r) = 1 + size l + size r
size Hole = 0

genTree :: forall g. MonadAutoGen g => g GBST
genTree = sized (aux Hole)
  where
    aux :: GBST -> Int -> g GBST
    aux t n = do
      t' <- growTree t
      if t == t' then pure t' else aux t' (n `div` 2)

growTree :: MonadAutoGen g => GBST -> g GBST
growTree = aux
  where
    aux Leaf = pure Leaf
    aux (Node l x r) = do
      l' <- aux l
      if l' == l
        then Node l x <$> aux r
        else pure $ Node l' x r
    aux Hole =
      select
        "NODE_TYPE"
        [ pure Leaf,
          select "NODE_VAL" (pure <$> [1 .. 10]) $ \x -> pure $ Node Hole x Hole
        ]
        pure