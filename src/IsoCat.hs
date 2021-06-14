{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}

module IsoCat where

import BST (BST (..))
import Control.Arrow (second)
import Control.Monad.Reader (MonadReader (..), ReaderT (runReaderT), asks)
import Control.Monad.State
import Control.Monad.Writer (MonadWriter (..), WriterT (runWriterT), execWriterT)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import QuickCheck.GenT (GenT, MonadGen (..), frequency, oneof, runGenT)
import STLCExample (Expr (..), Type (..), typeOf)
import qualified Test.QuickCheck as QC

type Iso a b = (a -> b, b -> Maybe a)

class (forall a. Functor (p a)) => Profunctor p where
  comap :: (u -> Maybe u') -> p u' v -> p u v

class (Profunctor p, forall a. Monad (p a)) => Profmonad p

(<$$>) :: forall p a b. Profunctor p => Iso a b -> p a a -> p b b
(f, b) <$$> p = comap b (fmap f p)

infixr 0 <$$>

(<**>) :: forall p a b. Profmonad p => p a a -> p b b -> p (a, b) (a, b)
px <**> py = do
  x <- comap mfst px
  y <- comap msnd py
  return (x, y)
  where
    mfst (x, _) = Just x
    msnd (_, y) = Just y

infixl 4 <**>

class (Profmonad g) => BiGen g where
  base :: Eq a => a -> g a a
  select :: String -> [g a a] -> g a a
  uniform :: [g a a] -> g a a

type Choice = (String, Int)

newtype Ungen b a = Ungen {runUngen :: ReaderT b (WriterT [Choice] Maybe) a}
  deriving (Functor, Applicative, Monad, MonadReader b, MonadWriter [Choice], MonadFail)

instance Profunctor Ungen where
  comap f g = do
    u <- ask
    case f u of
      Nothing -> fail ""
      Just u' -> Ungen . lift $ runReaderT (runUngen g) u'

instance Profmonad Ungen

firstJust :: Foldable t => t (Ungen r a) -> Ungen r a
firstJust ps =
  Ungen $ do
    r <- ask
    (x, c) <-
      lift . lift $
        foldr
          (mplus . (runWriterT . (`runReaderT` r) . runUngen))
          Nothing
          ps
    tell c
    pure x

instance BiGen Ungen where
  base x = ask >>= \y -> if x == y then pure x else fail ""
  select s ds =
    firstJust $
      zipWith
        (\i d -> tell [(s, i)] *> d)
        [0 ..]
        ds
  uniform = firstJust

newtype Gen b a = Gen {runGen :: QC.Gen a}
  deriving (Functor, Applicative, Monad, MonadGen)

instance Profunctor Gen where
  comap _ (Gen g) = Gen g

instance Profmonad Gen

instance BiGen Gen where
  base = pure
  select _ = oneof
  uniform = oneof

ungenerate :: Ungen a a -> a -> [Choice]
ungenerate u x = fromJust . execWriterT . (`runReaderT` x) . runUngen $ u

generate :: Gen a a -> IO a
generate = QC.generate . runGen

genTree :: forall g. BiGen g => g BST BST
genTree =
  select "Tree" [base Leaf, node <$$> genTree <**> genInt <**> genTree] -- TODO: Base
  where
    node = (\((l, x), r) -> Node l x r, \case Node l x r -> Just ((l, x), r); _ -> Nothing)
    genInt = select "Int" [base x | x <- [0 .. 10]]

funType :: Iso (Type, Type) Type
funType = (uncurry (:->:), \case t1 :->: t2 -> Just (t1, t2); _ -> Nothing)

lit :: Iso Int Expr
lit = (Lit, \case Lit l -> Just l; _ -> Nothing)

lam :: Iso (Type, Expr) Expr
lam = (uncurry Lam, \case Lam t e -> Just (t, e); _ -> Nothing)

app :: Iso (Expr, Expr) Expr
app = (uncurry (:@:), \case e1 :@: e2 -> Just (e1, e2); _ -> Nothing)

plus :: Iso (Expr, Expr) Expr
plus = (uncurry Plus, \case Plus e1 e2 -> Just (e1, e2); _ -> Nothing)

genType :: BiGen g => g Type Type
genType = aux (10 :: Int)
  where
    aux 0 = base TInt
    aux n = select "TYPE" [base TInt, funType <$$> aux (n `div` 2) <**> aux (n `div` 2)]

genExprOf :: BiGen g => Type -> g Expr Expr
genExprOf ty = arb ty [] (30 :: Int)
  where
    arb t ctx n = select "EXPR" $ [genForType t ctx n, varsOfType t ctx] ++ [genApp t ctx n | n /= 0]

    varsOfType t ctx = uniform [base (Var i) | (i, t') <- zip [0 ..] ctx, t' == t]

    cutType (_ :@: e) = typeOf e
    cutType _ = Nothing

    genApp t ctx n = do
      t' <- comap cutType genType
      app <$$> arb (t' :->: t) ctx (n `div` 2) <**> arb t' ctx (n `div` 2)

    genLit = lit <$$> select "LIT" [base x | x <- [-20 .. 20]]

    genForType TInt _ 0 = genLit
    genForType TInt ctx n =
      select
        "INT"
        [ genLit,
          plus <$$> arb TInt ctx (n `div` 2) <**> arb TInt ctx (n `div` 2)
        ]
    genForType (t1 :->: t2) ctx n = lam <$$> base t1 <**> arb t2 (t1 : ctx) n

genExpr :: BiGen g => g Expr Expr
genExpr = do
  t <- comap typeOf genType
  genExprOf t

type RLCtx = [(String, Int)]

type RLModel = Map (String, RLCtx) [Int]

newtype RLGen b a = RLGen {runRLGen :: GenT (ReaderT RLModel (State RLCtx)) a}
  deriving (Functor, Applicative, Monad, MonadGen)

instance MonadState RLCtx (RLGen b) where
  get = RLGen $ lift get
  put x = RLGen $ lift (put x)

instance MonadReader RLModel (RLGen b) where
  ask = RLGen $ lift ask
  local f (RLGen g) = do
    r <- ask
    s <- get
    (a, s') <- liftGen $ do
      x <- runGenT g
      let g' = local f x
      pure $ runState (runReaderT g' r) s
    put s'
    pure a

freqShuffle :: MonadGen g => [(Int, a)] -> g [a]
freqShuffle gs = do
  idxs <- aux (zip (map fst gs) [0 ..])
  return $ map (snd . (gs !!)) idxs
  where
    aux [] = return []
    aux is = do
      i <- frequency (map (second return) is)
      fmap (i :) (aux (filter ((/= i) . snd) is))

applyModel :: String -> [RLGen b a] -> RLGen b (Int, a)
applyModel s gs = do
  ctx <- get
  is <- asks (Map.findWithDefault [] (s, ctx))
  i <- frequency (zip is (pure <$> [0 ..]))
  a <- gs !! i
  pure (i, a)

instance Profunctor RLGen where
  comap _ (RLGen g) = RLGen g

instance Profmonad RLGen

instance BiGen RLGen where
  base = pure
  select s gs = do
    (i, g) <- applyModel s gs
    modify (take 4 . ((s, i) :))
    pure g

  uniform = oneof
