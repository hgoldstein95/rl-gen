{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Gen where

import Control.Lens (makeLenses, over, view, (^.))
import Control.Monad (ap, filterM, replicateM)
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.State (MonadState (..))
import Data.Default (Default (..))
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Maybe as Maybe
import Data.Ord (comparing)
import MonadGen (MonadGen (..), uniform)
import System.Random (Random (..), RandomGen (..), StdGen, newStdGen)

type SelectId = String

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

newtype Gen s a = MkGen {unGen :: s -> QCGen -> Int -> (a, s)}

instance Functor (Gen s) where
  fmap f (MkGen h) =
    MkGen
      ( \s r n ->
          let (x, s') = h s r n
           in (f x, s')
      )

instance Applicative (Gen s) where
  pure x = MkGen (\s _ _ -> (x, s))
  (<*>) = ap

instance Monad (Gen s) where
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

class Select s where
  type Ctx s :: *
  select :: SelectId -> Ctx s -> [s a] -> s a

instance Default s => Select (Gen s) where
  type Ctx (Gen s) = ()
  select _ _ = uniform

instance MonadState s (Gen s) where
  get = MkGen $ \s _ _ -> (s, s)
  put s = MkGen $ \_ _ _ -> ((), s)

instance Default s => MonadGen (Gen s) where
  sized f = MkGen (\s r n -> let MkGen m = f n in m s r n)

  resize n _ | n < 0 = error "Test.QuickCheck.resize: negative size"
  resize n (MkGen g) = MkGen (\s r _ -> g s r n)

  choose rng = MkGen (\s r _ -> let (x, _) = randomR rng r in (x, s))

  chooseAny = MkGen (\s r _ -> let (x, _) = random r in (x, s))

  generate (MkGen g) =
    do
      r <- newQCGen
      return (fst $ g def r 30)