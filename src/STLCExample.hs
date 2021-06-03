{-# LANGUAGE LambdaCase #-}

module STLCExample where

import Control.Monad.Reader (MonadReader (ask), ReaderT, guard, liftM2, local, runReaderT)
import Control.Monad.Trans (lift)
import Data.Maybe (fromJust, isJust)
import QuickCheck.GenT (GenT, MonadGen (liftGen), elements, oneof, runGenT, shuffle, sized)
import Test.QuickCheck (Gen, Property, forAll, quickCheck)

data Ty = TInt | Ty :->: Ty
  deriving (Show, Eq)

data Expr = Lit Int | Plus Expr Expr | Lam Ty Expr | Var Int | Expr :@: Expr
  deriving (Show, Eq)

typeOf :: Expr -> Maybe Ty
typeOf expr = runReaderT (aux expr) []
  where
    aux :: Expr -> ReaderT [Ty] Maybe Ty
    aux (Lit _) = pure TInt
    aux (Plus e1 e2) = do
      TInt <- aux e1
      TInt <- aux e2
      pure TInt
    aux (Lam t e) = do
      t' <- local (t :) (aux e)
      pure (t :->: t')
    aux (e1 :@: e2) = do
      (t1 :->: t2) <- aux e1
      t1' <- aux e2
      guard (t1 == t1')
      pure t2
    aux (Var n) = do
      ctx <- ask
      if length ctx <= n then lift Nothing else pure (ctx !! n)

select :: [GenT Maybe a] -> GenT Maybe a
select ds = lift =<< liftGen (aux =<< shuffle [0 .. length ds - 1])
  where
    aux (i : is) = do
      runGenT (ds !! i) >>= \case
        Nothing -> aux is
        Just x -> return (Just x)
    aux [] = return Nothing

genType :: GenT Maybe Ty
genType = sized aux
  where
    aux 0 = pure TInt
    aux n = select [pure TInt, liftM2 (:->:) (aux (n `div` 2)) (aux (n `div` 2))]

genExprOf :: Ty -> GenT Maybe Expr
genExprOf ty = sized (aux ty [])
  where
    aux t ctx 0 = select [genForType t ctx 0, varsOfType t ctx]
    aux t ctx n = select [genForType t ctx n, genApp t ctx n, varsOfType t ctx]

    varsOfType t ctx =
      -- needs nonstandard select
      let vs = [pure (Var x) | x <- map fst $ filter ((== t) . snd) $ zip [0 ..] ctx]
       in if null vs then lift Nothing else oneof vs

    genApp t ctx n =
      -- needs bind + depend
      genType >>= \t' ->
        liftM2 (:@:) (aux (t' :->: t) ctx (n `div` 2)) (aux t' ctx (n `div` 2))

    genForType TInt _ 0 = fmap Lit (elements [-20 .. 20])
    genForType TInt ctx n =
      select
        [ fmap Lit (elements [-20 .. 20]),
          liftM2 Plus (aux TInt ctx (n `div` 2)) (aux TInt ctx (n `div` 2))
        ]
    genForType (t1 :->: t2) ctx n = fmap (Lam t1) (aux t2 (t1 : ctx) n)

genExpr :: GenT Maybe Expr
genExpr =
  -- needs bind + depend
  genType >>= genExprOf

runGen :: GenT Maybe a -> Gen a
runGen = fmap fromJust . runGenT

prop_genExprOfOK :: Property
prop_genExprOfOK =
  forAll (runGen genType) $ \t ->
    forAll (runGen $ genExprOf t) $ \e ->
      Just t == typeOf e

prop_genExprOK :: Property
prop_genExprOK = forAll (runGen genExpr) $ \e -> isJust (typeOf e)

test :: IO ()
test = do
  quickCheck prop_genExprOfOK
  quickCheck prop_genExprOK