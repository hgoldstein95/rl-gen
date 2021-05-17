{-# LANGUAGE RankNTypes #-}

module Sandbox where

import Control.Monad.Cont (ContT (..))
import Control.Monad.Trans (lift)
import Test.QuickCheck (elements, generate)

shiftT :: (Monad m) => ((a -> m r) -> ContT r m r) -> ContT r m a
shiftT f = ContT ((`runContT` pure) . f)

resetT :: (Monad m) => ContT r m r -> ContT r' m r
resetT = lift . (`runContT` pure)

data BSTHole
  = Hole
  | Leaf
  | Node BSTHole Int BSTHole
  deriving (Show)

foo :: ContT BSTHole IO BSTHole
foo = aux (3 :: Int)
  where
    aux 0 = pure Leaf
    aux n = do
      x <- lift $ generate $ elements [1 .. 10]
      l <- aux (n - 1)
      r <- aux (n - 1)
      shiftT $ \k -> do
        lift $ print =<< k Hole
        lift $ k (Node l x r)

run :: ContT BSTHole IO BSTHole -> IO BSTHole
run c = runContT c pure