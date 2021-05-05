module BST where

import qualified Data.Set as Set
import Gen (Select (..))
import MCCGen (MCCContext (..), MCCGen (..), ctxAppend, reward)
import MonadGen (MonadGen (sized), infiniteListOf)

data BST
  = Node BST Int BST
  | Leaf
  deriving (Show, Ord, Eq)

toList :: BST -> [Int]
toList Leaf = []
toList (Node l x r) = toList l ++ [x] ++ toList r

genTree :: MCCContext -> MCCGen BST
genTree context = sized (aux context)
  where
    aux :: MCCContext -> Int -> MCCGen BST
    aux _ 0 = pure Leaf
    aux ctx n =
      select
        "NODE_TYPE"
        ctx
        [ pure Leaf,
          Node
            <$> aux (ctxAppend (-1) ctx) (n - 1)
            <*> genInt ctx
            <*> aux (ctxAppend (-2) ctx) (n - 1)
        ]

    genInt ctx = select "NODE_VAL" ctx (pure <$> [1 .. 10])

isBST :: BST -> Bool
isBST Leaf = True
isBST (Node l x r) = all (< x) (toList l) && all (> x) (toList r) && isBST l && isBST r

genTrees :: MCCGen [BST]
genTrees = infiniteListOf (genTree [])

genBSTs :: MCCGen [BST]
genBSTs = aux Set.empty
  where
    aux s = do
      t <- genTree []
      let bst = isBST t
          unique = not $ t `Set.member` s
      case (bst, unique) of
        (True, True) -> reward 20
        (True, False) -> reward 0
        _ -> reward (-1)
      (t :) <$> aux (if bst then Set.insert t s else s)
