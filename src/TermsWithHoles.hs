{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module TermsWithHoles where

import Test.QuickCheck (Gen, elements, frequency, sized)

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

genTree :: Gen GBST
genTree = sized (aux Hole)
  where
    aux t n = do
      t' <- growTree t
      if t == t' then pure t' else aux t' (n `div` 2)

growTree :: GBST -> Gen GBST
growTree t = aux t
  where
    aux Leaf = pure Leaf
    aux (Node l x r) = do
      l' <- aux l
      if l' == l
        then Node l x <$> aux r
        else pure $ Node l' x r
    aux Hole =
      frequency
        [ (size t, pure Leaf), -- distribution can depend on the bigger tree
          (1, (\x -> Node Hole x Hole) <$> elements [1 .. 10])
        ]