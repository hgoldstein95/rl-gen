{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import BST (BST (..))
import Control.Arrow (Arrow (second))
import MCCGen (MCCGen, mccselect')
import QuickCheck.GenT (Gen, MonadGen (..), oneof)

class MonadGen g => MonadAutoGen c g where
  select :: String -> c -> [g a] -> g a

instance MonadAutoGen (Focus BST) MCCGen where
  select t c gs = (gs !!) =<< mccselect' t (focusToContext c) gs
    where
      focusToContext (Focus (Node l x _, 0 : ds)) = take 4 $ focusToContext (Focus (l, ds)) ++ ["L", show x]
      focusToContext (Focus (Node _ x r, 1 : ds)) = take 4 $ focusToContext (Focus (r, ds)) ++ ["R", show x]
      focusToContext (Focus (Leaf, [])) = []
      focusToContext _ = error "ill-formed focus"

instance MonadAutoGen c Gen where
  select _ _ = oneof

grow :: (MonadAutoGen (Focus a) g, Grow a) => Int -> g a
grow maxDepth = expandPaths (terminal, [[]])
  where
    expandPaths (t, []) = pure t
    expandPaths (t, p : ps) | length p > maxDepth = expandPaths (t, ps) -- Cutoff for term that's too deep
    expandPaths (t, p : ps) = expandPaths . second (ps ++) =<< expand t p
      where
        expand n [] | n == terminal = getPotential <$> expandTerminal (Focus (t, p))
        expand n (d : ds) = do
          (t', ds') <- expand (extract d n) ds
          pure (insert d n t', (d :) <$> ds')
        expand _ _ = error "impossible"

type Path = [Int]

newtype Potential a = Potential {getPotential :: (a, [Path])}

newtype Focus a = Focus {getFocus :: (a, Path)}

withHoles :: Int -> a -> Potential a
withHoles n t = Potential (t, (: []) <$> [0 .. n -1])

class Eq a => Grow a where
  terminal :: a
  expandTerminal :: MonadAutoGen c g => c -> g (Potential a)
  extract :: Int -> a -> a
  insert :: Int -> a -> a -> a

instance Grow BST where
  terminal = Leaf

  extract 0 (Node l _ _) = l
  extract 1 (Node _ _ r) = r
  extract _ _ = error "failed to extract"

  insert 0 (Node _ x r) l = Node l x r
  insert 1 (Node l x _) r = Node l x r
  insert _ _ _ = error "failed to insert"

  expandTerminal c =
    select
      "NODE"
      c
      [ pure (withHoles 0 Leaf),
        (\x -> withHoles 2 (Node Leaf x Leaf)) <$> select "VAL" c (pure <$> [1 .. 100])
      ]