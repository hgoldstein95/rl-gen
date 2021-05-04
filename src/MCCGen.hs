{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Gen (Gen (MkGen), Select (..), SelectId, update)
import qualified Gen
import System.Random (Random (..), RandomGen (..), StdGen, newStdGen)

type MCCContext = [Int]

data MCCLearner = MCCLearner
  { _episode :: [(MCCContext, Int, Double)],
    _qTable :: Map (MCCContext, Int) Double,
    _cTable :: Map (MCCContext, Int) Double
  }

data MCCState = MCCState
  { _contexts :: Map SelectId MCCContext,
    _learners :: Map SelectId MCCLearner
  }

makeLenses ''MCCState
makeLenses ''MCCLearner

newtype MCCGen a = MCCGen {unMCC :: Gen MCCState a}
  deriving (Functor, Applicative, Monad)

instance Default MCCLearner where
  def = MCCLearner [] Map.empty Map.empty

instance Default MCCState where
  def = MCCState Map.empty Map.empty

generate :: MCCGen a -> IO a
generate = Gen.generate . unMCC

uniform :: [MCCGen a] -> MCCGen a
uniform (map unMCC -> gs) = MCCGen $ Gen.uniform gs

frequency :: [(Int, MCCGen a)] -> MCCGen a
frequency = MCCGen . Gen.frequency . map (second unMCC)

floatFrequency :: [(Double, MCCGen a)] -> MCCGen a
floatFrequency = MCCGen . Gen.frequency . map (\(f, g) -> (round (f * 100), unMCC g))

choose :: Random a => (a, a) -> MCCGen a
choose c = MCCGen $ Gen.choose c

get :: MCCGen MCCState
get = MCCGen Gen.get

vectorOf :: Int -> MCCGen a -> MCCGen [a]
vectorOf = replicateM

sized :: (Int -> MCCGen a) -> MCCGen a
sized f = MCCGen $ MkGen (\s r n -> let (MCCGen (MkGen m)) = f n in m s r n)

instance Select MCCGen where
  select selId gs = do
    e <- choose (0 :: Int, 3)
    if e == 0
      then uniform gs
      else do
        ctx <- fromJust . view (contexts . at selId) <$> get
        qs <- view qTable . Maybe.fromMaybe def . view (learners . at selId) <$> get
        floatFrequency $ zip (map (\i -> Map.findWithDefault 1 (ctx, i) qs) [0 .. length gs]) gs

rewardOne :: SelectId -> Double -> MCCGen ()
rewardOne selId rv = MCCGen $ update (over learners (Map.update (Just . rewardLearner) selId))
  where
    rewardLearner :: MCCLearner -> MCCLearner
    rewardLearner learner =
      aux (over (ix 0 . _3) (const rv) . reverse $ learner ^. episode) 0 learner
      where
        aux [] g l = l
        aux ((s, a, r) : es) g l =
          let g' = g * r
              cTable' = Map.update (Just . (+ 1)) (s, a) (l ^. cTable)
              qTable' = Map.update (\q -> Just $ q + 1 / Map.findWithDefault 1 (s, a) cTable' + g' - q) (s, a) (l ^. qTable)
           in aux es g' (MCCLearner es qTable' cTable')

reward :: Double -> MCCGen ()
reward rv = do
  selIds <- Map.keys . view learners <$> get
  mapM_ (`rewardOne` rv) selIds