{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module MCCGen where

import Control.Arrow (Arrow (second))
import Control.Lens (at, ix, makeLenses, non, over, set, view, (%%=), (%=), (^.), _3)
import Control.Monad (ap, filterM, replicateM)
import Control.Monad.Fix (MonadFix (..))
import Control.Monad.State (MonadState (..), gets, modify)
import Data.Default (Default (..))
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Maybe as Maybe
import Data.Ord (comparing)
import Debug.Trace (traceShowId)
import GHC.Generics (Generic)
import Gen (Gen (MkGen), Select (..), SelectId)
import qualified Gen
import MonadGen (MonadGen (..), elements, frequency, uniform)
import System.Random (Random (..), RandomGen (..), StdGen, newStdGen)

type MCCContext = [String]

data MCCLearner = MCCLearner
  { _episode :: [(MCCContext, Int, Double)],
    _qTable :: Map (MCCContext, Int) Double,
    _cTable :: Map (MCCContext, Int) Double
  }
  deriving (Show, Eq)

newtype MCCState = MCCState
  { _learners :: Map SelectId MCCLearner
  }
  deriving (Show, Eq)

makeLenses ''MCCState
makeLenses ''MCCLearner

newtype MCCGen a = MCCGen {unMCC :: Gen MCCState a}
  deriving (Functor, Applicative, Monad, MonadGen)

instance MonadState MCCState MCCGen where
  get = MCCGen get
  put s = MCCGen (put s)

instance Default MCCLearner where
  def = MCCLearner [] Map.empty Map.empty

instance Default MCCState where
  def = MCCState Map.empty

instance Select MCCGen where
  type Ctx MCCGen = MCCContext
  select selId ctx gs = do
    e <- choose (0 :: Int, 3)
    let idxs = [0 .. length gs - 1]
    c <-
      if e == 0
        then elements idxs
        else do
          qs <- gets (view (learners . at selId . non def . qTable))
          let qGens = map (\i -> (qs ^. (at (ctx, i) . non 0), i)) idxs
              maxq = maximum (map fst qGens)
          elements . map snd . filter ((== maxq) . fst) $ qGens
    learners . at selId . non def . episode %= (++ [(ctx, c, 0)])
    gs !! c

(<:>) :: String -> MCCContext -> MCCContext
(<:>) x = (x :) . take 3

reward :: Double -> MCCGen ()
reward rv = do
  selIds <- gets (Map.keys . view learners)
  mapM_ (`rewardOne` rv) selIds

rewardOne :: SelectId -> Double -> MCCGen ()
rewardOne selId rv = learners . at selId . non def %= rewardLearner
  where
    rewardLearner learner =
      aux (set (ix 0 . _3) rv . reverse $ learner ^. episode) 0 learner
      where
        aux [] _ l = l
        aux ((s, a, r) : es) g l =
          let g' = g + r
              cTable' = over (at (s, a) . non 0) (+ 1) (l ^. cTable)
              qTable' = over (at (s, a) . non 0) (\q -> q + (1 / fromJust (cTable' ^. at (s, a))) * (g' - q)) (l ^. qTable)
           in aux es g' (MCCLearner es qTable' cTable')