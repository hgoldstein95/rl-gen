{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module MCCGen where

import Control.Arrow (first, second)
import Control.Lens
  ( At (at),
    Field3 (_3),
    Ixed (ix),
    makeLenses,
    non,
    over,
    set,
    view,
    (%=),
    (^.),
  )
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State.Lazy (MonadState (..), StateT, evalStateT, gets, modify)
import Control.Monad.Trans (MonadTrans (..))
import Data.Default (Default (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import QuickCheck.GenT
  ( GenT,
    choose,
    elements,
    runGenT,
  )
import qualified Test.QuickCheck as QC

type SelectId = String

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

instance Default MCCLearner where
  def = MCCLearner [] Map.empty Map.empty

instance Default MCCState where
  def = MCCState Map.empty

type MCCGen = GenT (StateT (MCCContext, MCCState) [])

generate :: MCCGen a -> IO a
generate = ((head . (`evalStateT` def)) <$>) . QC.generate . runGenT

instance MonadState MCCState MCCGen where
  get = lift (gets snd)
  put s = lift . modify $ second (const s)

instance MonadReader MCCContext MCCGen where
  ask = lift (gets fst)
  local f g = do
    ctx <- lift $ do
      ctx <- gets fst
      modify (first f)
      pure ctx
    x <- g
    lift . modify $ first (const ctx)
    pure x

mccselect :: SelectId -> [MCCGen a] -> MCCGen a
mccselect selId gs = do
  e <- choose (0 :: Int, 3)
  ctx <- ask
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
              qTable' = over (at (s, a) . non 0) (\q -> q + (1 / Maybe.fromJust (cTable' ^. at (s, a))) * (g' - q)) (l ^. qTable)
           in aux es g' (MCCLearner es qTable' cTable')