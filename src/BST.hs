module BST where

import Control.Monad.Reader (MonadReader (..))
import qualified Data.Set as Set
import MCCGen (MCCGen, generate, mccselect, reward, (<:>))
import QuickCheck.GenT
  ( infiniteListOf,
    resize,
    sized,
  )

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

genTree :: MCCGen BST
genTree = resize 4 $ sized aux
  where
    aux :: Int -> MCCGen BST
    aux 0 = pure Leaf
    aux depth = do
      i <- mccselect "NODE_VAL" (pure <$> [0 .. 10])
      local (("VAL(" ++ show i ++ ")") <:>) $
        mccselect
          "NODE_TYPE"
          [ pure Leaf,
            Node
              <$> local ("LEFT" <:>) (aux (depth - 1))
              <*> pure i
              <*> local ("RIGHT" <:>) (aux (depth - 1))
          ]

genTrees :: MCCGen [BST]
genTrees = infiniteListOf genTree

genBSTs :: MCCGen [BST]
genBSTs = aux Set.empty
  where
    aux s = do
      t <- genTree
      let bst = isBST t
          unique = not $ t `Set.member` s
      reward $ case (bst, unique) of
        (True, True) -> 20
        (True, False) -> 0
        _ -> -1
      ts <- aux (Set.insert t s)
      pure (t : ts)

test :: Int -> IO ()
test total = do
  -- aux "RAND:" genTrees
  aux "RL  :" genBSTs
  where
    aux n g = do
      putStr n
      ts <- filter isBST <$> generate (take total <$> g)
      putStr $ " " ++ show (length ts) ++ " valid BSTs"
      putStr $ ", " ++ show (length . Set.fromList $ ts) ++ " unique valid BSTs"
      putStr $ ", " ++ show (maximum . map size $ ts) ++ " max size"
      putStrLn ""