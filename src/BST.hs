module BST where

import qualified Data.Set as Set
import MCCGen (MCCContext, MCCGen (..), reward, (<:>))
import MonadGen (MonadGen (generate, resize, sized), Select (..), infiniteListOf)

data BST
  = Node BST Int BST
  | Leaf
  deriving (Show, Ord, Eq)

toList :: BST -> [Int]
toList Leaf = []
toList (Node l x r) = toList l ++ [x] ++ toList r

size :: BST -> Int
size = length . toList

isBST :: BST -> Bool
isBST Leaf = True
isBST (Node l x r) = all (< x) (toList l) && all (> x) (toList r) && isBST l && isBST r

genTree :: MCCContext -> MCCGen BST
genTree context = resize 4 $ sized (aux context)
  where
    aux :: MCCContext -> Int -> MCCGen BST
    aux _ 0 = pure Leaf
    aux ctx depth = do
      i <- select "NODE_VAL" ctx (pure <$> [0 .. 10])
      let ctx' = ("VAL(" ++ show i ++ ")") <:> ctx
      select
        "NODE_TYPE"
        ctx'
        [ pure Leaf,
          Node
            <$> aux ("LEFT" <:> ctx') (depth - 1)
            <*> pure i
            <*> aux ("RIGHT" <:> ctx') (depth - 1)
        ]

genTrees :: MCCGen [BST]
genTrees = infiniteListOf (genTree [])

genBSTs :: MCCGen [BST]
genBSTs = aux Set.empty
  where
    aux s = do
      t <- genTree []
      let bst = isBST t
          unique = not $ t `Set.member` s
      reward $ case (bst, unique) of
        (True, True) -> 20
        (True, False) -> 0
        _ -> -1
      (t :) <$> aux (Set.insert t s)

test :: Int -> IO ()
test total = do
  aux "RAND:" genTrees
  aux "RL  :" genBSTs
  where
    aux n g = do
      putStr n
      ts <- filter isBST <$> generate (take total <$> g)
      putStr $ " " ++ show (length ts) ++ " valid BSTs"
      putStr $ ", " ++ show (length . Set.fromList $ ts) ++ " unique valid BSTs"
      putStr $ ", " ++ show (maximum . map size $ ts) ++ " max size"
      putStrLn ""