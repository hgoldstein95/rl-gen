{-# LANGUAGE TupleSections #-}

module Stephen where

import BST (BST (..))
import Test.QuickCheck

genTree :: Gen BST
genTree = sized aux
  where
    aux :: Int -> Gen BST
    aux 0 = pure Leaf
    aux n = do
      i <- elements [1 .. 10]
      frequency
        [ (1, pure Leaf),
          ( i,
            do
              l <- aux (n `div` 2)
              let x = i
              r <- aux (n `div` 2)
              pure (Node l x r)
          )
        ]

genTree' :: Gen BST
genTree' = pure (Node (Node Leaf 42 Leaf) 10 Leaf)

leaf :: BST
leaf = Leaf

mLeaf :: Gen BST
mLeaf = return Leaf

node :: BST -> Int -> BST -> BST
node = Node

mNode :: Gen BST -> Gen Int -> Gen BST -> Gen BST
mNode l x r = Node <$> l <*> x <*> r