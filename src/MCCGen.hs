{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module MCCGen where

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
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState (..), State, evalState, gets)
import qualified Control.Monad.State as State
import Data.Default (Default (..))
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import GenT
  ( GenT (MkGenT),
    choose,
    elements,
  )
import qualified GenT

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

type MCCGen = GenT (ReaderT MCCContext (State MCCState))

generate :: MCCGen a -> IO a
generate = GenT.generate ((`evalState` def) . (`runReaderT` []))

instance MonadState MCCState MCCGen where
  get = MkGenT (\_ _ -> State.get)
  put s = MkGenT (\_ _ -> State.put s)

instance MonadReader MCCContext MCCGen where
  ask = MkGenT (\_ _ -> Reader.ask)
  local f (MkGenT g) = MkGenT (\s n -> Reader.local f (g s n))

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