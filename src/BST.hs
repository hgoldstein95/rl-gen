module BST where

import qualified Data.Set as Set
import Gen (Select (..))
import MCCGen (MCCGen, reward, sized)

data BST
  = Node BST Int BST
  | Leaf
  deriving (Show, Ord, Eq)

toList :: BST -> [Int]
toList Leaf = []
toList (Node l x r) = toList l ++ [x] ++ toList r

genTree :: MCCGen BST
genTree = sized aux
  where
    aux 0 = pure Leaf
    aux n = select "NODE_TYPE" [pure Leaf, Node <$> aux (n - 1) <*> genInt <*> aux (n - 1)]

    genInt = select "NODE_VAL" (pure <$> [1 .. 10])

isBST :: BST -> Bool
isBST Leaf = True
isBST (Node l x r) = all (< x) (toList l) && all (> x) (toList r) && isBST l && isBST r

genBST :: MCCGen BST
genBST = aux Set.empty
  where
    aux s = do
      t <- genTree
      case (isBST t, t `Set.member` s) of
        (True, True) -> reward 20
        (True, False) -> reward 0
        _ -> reward (-1)
      pure t