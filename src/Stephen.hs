{-# LANGUAGE TupleSections #-}

module Stephen where

import BST (BST (..))
import Test.QuickCheck

genTree :: Gen BST
genTree =
  frequency
    [ (1, pure Leaf),
      ( 10,
        do
          l <- genTree
          x <- elements [1 .. 10]
          r <- genTree
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