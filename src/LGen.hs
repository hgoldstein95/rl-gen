{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module LGen where

import Control.Monad (ap)
import Control.Monad.State (MonadState (..))
import Data.Default (Default (..))
import MonadGen (MonadGen (..), Select (..), uniform)
import System.Random (Random (..), RandomGen (..), StdGen, newStdGen)

newtype QCGen = QCGen StdGen

newQCGen :: IO QCGen
newQCGen = fmap QCGen newStdGen

instance RandomGen QCGen where
  split (QCGen g) =
    case split g of
      (g1, g2) -> (QCGen g1, QCGen g2)
  genRange (QCGen g) = genRange g
  next = wrapQCGen next
    where
      wrapQCGen f (QCGen g) =
        case f g of
          (x, g') -> (x, QCGen g')

newtype LGen s a = MkGen {unGen :: s -> QCGen -> Int -> (a, s)}

instance Functor (LGen s) where
  fmap f (MkGen h) =
    MkGen
      ( \s r n ->
          let (x, s') = h s r n
           in (f x, s')
      )

instance Applicative (LGen s) where
  pure x = MkGen (\s _ _ -> (x, s))
  (<*>) = ap

instance Monad (LGen s) where
  return = pure
  MkGen m >>= k =
    MkGen
      ( \s r n ->
          case split r of
            (r1, r2) ->
              let (x, s') = m s r1 n
                  MkGen m' = k x
               in m' s' r2 n
      )

instance Default s => Select (LGen s) where
  type Ctx (LGen s) = ()
  select _ _ = uniform

instance MonadState s (LGen s) where
  get = MkGen $ \s _ _ -> (s, s)
  put s = MkGen $ \_ _ _ -> ((), s)

instance Default s => MonadGen (LGen s) where
  sized f = MkGen (\s r n -> let MkGen m = f n in m s r n)

  resize n _ | n < 0 = error "Test.QuickCheck.resize: negative size"
  resize n (MkGen g) = MkGen (\s r _ -> g s r n)

  choose rng = MkGen (\s r _ -> let (x, _) = randomR rng r in (x, s))

  chooseAny = MkGen (\s r _ -> let (x, _) = random r in (x, s))

  generate (MkGen g) =
    do
      r <- newQCGen
      return (fst $ g def r 30)