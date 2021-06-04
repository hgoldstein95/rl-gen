{-# LANGUAGE LambdaCase #-}

module STLCExample where

import Control.Monad.Reader (MonadReader (ask), ReaderT, guard, local, runReaderT)
import Control.Monad.Trans (lift)
import Data.Maybe (isJust)
import ParserGen
  ( Category ((.)),
    Iso (..),
    IsoFunctor ((<$>)),
    ProductFunctor ((<*>)),
    Syntax (..),
    depend,
    generate_,
  )
import Test.QuickCheck (Property, forAll, quickCheck)
import Prelude hiding (id, pure, sum, (*>), (.), (<$>), (<*), (<*>))

data Ty = TInt | Ty :->: Ty
  deriving (Show, Eq)

funTy :: Iso (Ty, Ty) Ty
funTy =
  Iso
    (Just . uncurry (:->:))
    ( \case
        t1 :->: t2 -> Just (t1, t2)
        _ -> Nothing
    )

data Expr = Lit Int | Plus Expr Expr | Lam Ty Expr | Var Int | Expr :@: Expr
  deriving (Show, Eq)

lit :: Iso Int Expr
lit =
  Iso
    (Just . Lit)
    ( \case
        Lit l -> Just l
        _ -> Nothing
    )

lam :: Iso (Ty, Expr) Expr
lam =
  Iso
    (Just . uncurry Lam)
    ( \case
        Lam t e -> Just (t, e)
        _ -> Nothing
    )

app :: Iso (Expr, Expr) Expr
app =
  Iso
    (Just . uncurry (:@:))
    ( \case
        e1 :@: e2 -> Just (e1, e2)
        _ -> Nothing
    )

plus :: Iso (Expr, Expr) Expr
plus =
  Iso
    (Just . uncurry Plus)
    ( \case
        Plus e1 e2 -> Just (e1, e2)
        _ -> Nothing
    )

typeOf :: Expr -> Maybe Ty
typeOf expr = runReaderT (aux expr) []
  where
    aux :: Expr -> ReaderT [Ty] Maybe Ty
    aux (Lit _) = return TInt
    aux (Plus e1 e2) = do
      TInt <- aux e1
      TInt <- aux e2
      return TInt
    aux (Lam t e) = do
      t' <- local (t :) (aux e)
      return (t :->: t')
    aux (e1 :@: e2) = do
      (t1 :->: t2) <- aux e1
      t1' <- aux e2
      guard (t1 == t1')
      return t2
    aux (Var n) = do
      ctx <- ask
      if length ctx <= n then lift Nothing else return (ctx !! n)

genType :: Syntax d => d Ty
genType = aux (10 :: Int)
  where
    aux 0 = pure TInt
    aux n = select "TYPE" [pure TInt, funTy <$> (aux (n `div` 2) <*> aux (n `div` 2))]

genExprOf :: Syntax d => Ty -> d Expr
genExprOf ty = aux ty [] (30 :: Int)
  where
    aux t ctx n = select "EXPR" [genForType t ctx n, genApp t ctx n, varsOfType t ctx]

    varsOfType t ctx = uniform [Var x | x <- map fst $ filter ((== t) . snd) $ zip [0 ..] ctx]

    cutType (_ :@: e) = typeOf e
    cutType _ = Nothing

    genApp _ _ 0 = empty
    genApp t ctx n =
      depend cutType
        <$> bind
          genType
          ( \t' ->
              app <$> (aux (t' :->: t) ctx (n `div` 2) <*> aux t' ctx (n `div` 2))
          )

    genLit = lit <$> select "LIT" (fmap pure [-20 .. 20])

    genForType TInt _ 0 = genLit
    genForType TInt ctx n =
      select
        "INT"
        [ genLit,
          plus <$> (aux TInt ctx (n `div` 2) <*> aux TInt ctx (n `div` 2))
        ]
    genForType (t1 :->: t2) ctx n = lam <$> (pure t1 <*> aux t2 (t1 : ctx) n)

genExpr :: Syntax d => d Expr
genExpr = depend typeOf <$> bind genType genExprOf

prop_genExprOfOK :: Property
prop_genExprOfOK =
  forAll (generate_ genType) $ \t ->
    forAll (generate_ $ genExprOf t) $ \e ->
      Just t == typeOf e

prop_genExprOK :: Property
prop_genExprOK = forAll (generate_ genExpr) $ \e -> isJust (typeOf e)

test :: IO ()
test = do
  quickCheck prop_genExprOfOK
  quickCheck prop_genExprOK