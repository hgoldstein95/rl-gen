module BST where

import Control.Monad (replicateM)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (StateT, evalStateT, get, modify)
import Control.Monad.Trans (lift)
import Data.Set (Set)
import qualified Data.Set as Set
import MCCGen (MCCGen, generate, mccselect, reward, (<:>))
import QuickCheck.GenT (resize, sized)

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

genBST :: StateT (Set BST) MCCGen BST
genBST = do
  t <- lift genTree
  s <- get
  let bst = isBST t
      unique = not $ t `Set.member` s
  lift $
    reward $ case (bst, unique) of
      (True, True) -> 20
      (True, False) -> 0
      _ -> -1
  modify (Set.insert t)
  pure t

genTrees :: Int -> MCCGen [BST]
genTrees n = replicateM n genTree

genBSTs :: Int -> MCCGen [BST]
genBSTs n = evalStateT (replicateM n genBST) Set.empty

test :: Int -> IO ()
test total = do
  aux "RAND:" (genTrees total)
  aux "RL  :" (genBSTs total)
  where
    aux n g = do
      putStr n
      ts <- filter isBST <$> generate g
      putStr $ " " ++ show (length ts) ++ " valid BSTs"
      putStr $ ", " ++ show (length . Set.fromList $ ts) ++ " unique valid BSTs"
      putStr $ ", " ++ show (maximum . map size $ ts) ++ " max size"
      putStrLn ""