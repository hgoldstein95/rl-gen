{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module MCCGen where

import Control.Arrow (Arrow (second))
import Control.Lens (at, ix, makeLenses, over, view, (^.), _3, _Just)
import Control.Monad (ap, filterM, replicateM)
import Control.Monad.Fix (MonadFix (..))
import Data.Default (Default (..))
import Data.List (sortBy)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import qualified Data.Maybe as Maybe
import Data.Ord (comparing)
import Gen (Gen (MkGen), Select (..), SelectId)
import qualified Gen
import MonadGen (MonadGen (..), frequency, uniform)
import System.Random (Random (..), RandomGen (..), StdGen, newStdGen)

type MCCContext = [Int]

data MCCLearner = MCCLearner
  { _episode :: [(MCCContext, Int, Double)],
    _qTable :: Map (MCCContext, Int) Double,
    _cTable :: Map (MCCContext, Int) Double
  }

newtype MCCState = MCCState
  { _learners :: Map SelectId MCCLearner
  }

makeLenses ''MCCState
makeLenses ''MCCLearner

newtype MCCGen a = MCCGen {unMCC :: Gen MCCState a}
  deriving (Functor, Applicative, Monad, MonadGen)

instance Default MCCLearner where
  def = MCCLearner [] Map.empty Map.empty

instance Default MCCState where
  def = MCCState Map.empty

floatFrequency :: [(Double, MCCGen a)] -> MCCGen a
floatFrequency = frequency . map (\(f, g) -> (round (f * 100), g))

get :: MCCGen MCCState
get = MCCGen Gen.get

update :: (MCCState -> MCCState) -> MCCGen ()
update f = MCCGen (Gen.update f)

instance Select MCCGen where
  type Ctx MCCGen = MCCContext
  select selId ctx gs = do
    e <- choose (0 :: Int, 3)
    let idxs = pure <$> [0 .. length gs - 1]
    c <-
      if e == 0
        then uniform idxs
        else do
          qs <- view qTable . Maybe.fromMaybe def . view (learners . at selId) <$> get
          floatFrequency $ zip (map (\i -> Map.findWithDefault 1 (ctx, i) qs) [0 .. length gs - 1]) idxs
    update (over learners (updateWithDefault def (Just . over episode (++ [(ctx, c, 0)])) selId))
    gs !! c

ctxAppend :: Int -> MCCContext -> MCCContext
ctxAppend x = (x :) . take 3

rewardOne :: SelectId -> Double -> MCCGen ()
rewardOne selId rv = update (over learners (updateWithDefault def (Just . rewardLearner) selId))
  where
    rewardLearner learner =
      aux (over (ix 0 . _3) (const rv) . reverse $ learner ^. episode) 0 learner
      where
        aux [] g l = l
        aux ((s, a, r) : es) g l =
          let g' = g * r
              cTable' = updateWithDefault 0 (Just . (+ 1)) (s, a) (l ^. cTable)
              qTable' = updateWithDefault 0 (\q -> Just $ q + 1 / fromJust (Map.lookup (s, a) cTable') + g' - q) (s, a) (l ^. qTable)
           in aux es g' (MCCLearner es qTable' cTable')

updateWithDefault :: (Eq k, Ord k) => v -> (v -> Maybe v) -> k -> Map k v -> Map k v
updateWithDefault d f k m =
  let m' = if k `elem` Map.keys m then m else Map.insert k d m
   in Map.update f k m'

reward :: Double -> MCCGen ()
reward rv = do
  selIds <- Map.keys . view learners <$> get
  mapM_ (`rewardOne` rv) selIds