{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-methods -Wno-partial-type-signatures #-}

module SystemF
  ( Mutant (..),
    Type (..),
    Expr (..),
    wellFormedType,
    typeOf,
    peval,
    size,
    wellTyped,
    genExpr,
    test,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (guard)
import Data.Data (Data, Typeable)
import Data.List (nub)
import Data.Maybe (isJust)
import GHC.Generics (Generic)
import ParserGen
  ( Category ((.)),
    Iso (..),
    IsoFunctor ((<$>)),
    MGen (unMGen),
    ProductFunctor ((<*>)),
    Syntax (bind, pure, select, uniform),
    depend,
    generate_,
  )
import QuickCheck.GenT (runGenT)
import Test.QuickCheck (Property, forAll, quickCheck, (==>))
import Text.Printf
  ( FieldFormat (fmtChar, fmtPrecision),
    PrintfArg (formatArg),
    errorBadFormat,
    formatString,
    vFmt,
  )
import Prelude hiding (foldl, id, iterate, pure, sum, (*>), (.), (<$>), (<*), (<*>))

------------------------------------------
-- DEFINITIONS
------------------------------------------

{- begin type -}
data Type = Base | TBool | Type :-> Type | TVar Int | ForAll Type
  {- end type -}
  deriving (Eq, Ord, Generic)

{- begin expr -}
data Expr
  = Con
  | Var Int
  | Lam Type Expr
  | Expr :@: Expr
  | Cond Expr Expr Expr
  | BTrue
  | BFalse
  | TLam Expr
  | TApp Expr Type
  {- end expr -}
  deriving (Eq, Ord, Generic)

instance NFData Type

instance NFData Expr

------------------------------------------
-- PRINTING
------------------------------------------

data Precedence = Outer | App | Inner
  deriving (Eq, Ord)

instance Show Type where
  show t = showType' Outer t

showType' :: Precedence -> Type -> [Char]
showType' _ Base = "()"
showType' _ TBool = "B"
showType' _ (TVar n) = show n
showType' p (ForAll t) = parens p Outer $ "forall " ++ showType' Outer t
showType' p (t1 :-> t2) = parens p App $ showType' Inner t1 ++ "->" ++ showType' App t2

instance Show Expr where
  show e = show' Outer e

show' :: Precedence -> Expr -> [Char]
show' _ Con = "#"
show' _ BTrue = "T"
show' _ BFalse = "F"
show' _ (Var x) = show x
show' p (Lam t e) = parens p Outer $ "lam " ++ show t ++ ". " ++ show' Outer e
show' p (TLam e) = parens p Outer $ "Lam. " ++ show' Outer e
show' p (e1 :@: e2) = parens p App $ show' App e1 ++ " " ++ show' Inner e2
show' p (TApp e1 t2) = parens p App $ show' App e1 ++ " [" ++ showType' Outer t2 ++ "]"
show' p (Cond e1 e2 e3) =
  parens p Inner $
    "if " ++ show' Outer e1 ++ " then " ++ show' Outer e2
      ++ " else "
      ++ show' Outer e3

parens :: Ord a => a -> a -> [Char] -> [Char]
parens outer inner s = if outer > inner then "(" ++ s ++ ")" else s

------------------------------------------
-- MUTANTS
------------------------------------------

data Mutant
  = NoMutant
  | LiftVar
  | LiftLam
  | LiftTLamA
  | LiftTLamB
  | LiftTApp
  | SubstVar
  | SubstLT
  | SubstApp
  | SubstInTypeNoLift
  | SubstInTypeNoDecr
  | SubstInTypeLT
  | LiftTypeTVar
  | LiftTypeForAll
  | TSubstNoLift
  | TSubstNoIncr
  | AppForgetSubst
  | TAppForgetSubst
  | SubstSwapped
  | SubstNoIncr
  | SubstNoLift
  | SubstInTypeNoIncr
  | SubstNoLiftT
  | LiftTNoIncr
  | CondFalseToTrue
  deriving (Show, Eq, Ord, Enum, Bounded, Data, Typeable)

instance PrintfArg Mutant where
  formatArg x fmt
    | fmtChar (vFmt 's' fmt) == 's' = formatString (show x) (fmt {fmtChar = 's', fmtPrecision = Nothing})
  formatArg _ fmt = errorBadFormat $ fmtChar fmt

mut :: (Eq a, ?mutant :: a) => a -> p -> p -> p
mut m good bad = if m == ?mutant then bad else good

-- TYPECHECKING
------------------------------------------

-- | I can't believe we had to write this
nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth 0 (a : _) = Just a
nth n (_ : as) = nth (n -1) as

wellFormedType :: Int -> Type -> Bool
wellFormedType _ Base = True
wellFormedType _ TBool = True
wellFormedType ftv (TVar n) = n < ftv && n >= 0
-- J: added latter condition to avoid counterexamples with free type variables
wellFormedType ftv (t1 :-> t2) = wellFormedType ftv t1 && wellFormedType ftv t2
wellFormedType ftv (ForAll t) = wellFormedType (ftv + 1) t

-- TODO: Pass a Config (maybe not needed??)
typeOf' :: _ => Int -> [Type] -> Expr -> Maybe Type
typeOf' _ _ Con = Just Base
typeOf' _ _ BTrue = Just TBool
typeOf' _ _ BFalse = Just TBool
typeOf' ftv c (Lam t e) = guard (wellFormedType ftv t) >> fmap (t :->) (typeOf' ftv (t : c) e)
typeOf' ftv c (TLam e) = fmap ForAll (typeOf' (ftv + 1) (map (liftType 0) c) e)
typeOf' _ c (Var n) = nth n c
typeOf' ftv c (TApp e t) = do
  t2 <- typeOf' ftv c e
  guard $ wellFormedType ftv t
  case t2 of
    ForAll t2' -> Just $ substInType 0 t t2'
    _ -> Nothing
typeOf' ftv c (e1 :@: e2) = do
  t12 <- typeOf' ftv c e1
  t1' <- typeOf' ftv c e2
  case t12 of
    t1 :-> t2 -> do
      guard (t1 == t1')
      Just t2
    _ -> Nothing
typeOf' ftv c (Cond e1 e2 e3) = do
  t1 <- typeOf' ftv c e1
  t2 <- typeOf' ftv c e2
  t3 <- typeOf' ftv c e3
  guard (t1 == TBool && t2 == t3)
  Just t2

typeOf :: Expr -> Maybe Type
typeOf = let ?mutant = NoMutant in typeOf' 0 []

------------------------------------------
-- SUBSTITUTION
------------------------------------------

subst :: _ => Int -> Expr -> Expr -> Expr
subst _ _ Con = Con
subst _ _ BTrue = BTrue
subst _ _ BFalse = BFalse
subst n s (Var x)
  | x == n = s
  | mut SubstLT (x > n) (x < n) = Var $ mut SubstVar (x -1) x
  | otherwise = Var x
subst n s (Lam t e) = Lam t $ subst (mut SubstNoIncr (n + 1) n) (mut SubstNoLift (lift 0 s) s) e
subst n s (TLam e) = TLam $ subst n (mut SubstNoLiftT (liftTypes 0 s) s) e
subst n s (e1 :@: e2) = mut SubstApp (subst n s e1) (subst n e1 s) :@: subst n s e2
subst n s (TApp e1 t2) = TApp (subst n s e1) t2
subst n s (Cond e1 e2 e3) = Cond (subst n s e1) (subst n s e2) (subst n s e3)

lift :: _ => Int -> Expr -> Expr
lift _ Con = Con
lift _ BTrue = BTrue
lift _ BFalse = BFalse
lift n (Var x) = Var $ mut LiftVar (if x < n then x else x + 1) (x + 1)
lift n (Lam t e) = Lam t $ lift (mut LiftLam (n + 1) n) e
lift n (TLam e) = TLam $ mut LiftTLamA (lift (mut LiftTLamB n (n + 1)) e) e
lift n (e1 :@: e2) = lift n e1 :@: lift n e2
lift n (TApp e1 t2) = TApp (mut LiftTApp (lift n e1) e1) t2
lift n (Cond e1 e2 e3) = Cond (lift n e1) (lift n e2) (lift n e3)

-- Increase type annotations above n
liftTypes :: (?mutant :: Mutant) => Int -> Expr -> Expr
liftTypes _ Con = Con
liftTypes _ BTrue = BTrue
liftTypes _ BFalse = BFalse
liftTypes _ (Var x) = Var x
liftTypes n (Lam t e) = Lam (liftType n t) $ liftTypes n e
liftTypes n (TLam e) = TLam $ liftTypes (mut LiftTNoIncr (n + 1) n) e
liftTypes n (e1 :@: e2) = liftTypes n e1 :@: liftTypes n e2
liftTypes n (TApp e1 t2) = TApp (liftTypes n e1) (liftType n t2)
liftTypes n (Cond e1 e2 e3) = Cond (liftTypes n e1) (liftTypes n e2) (liftTypes n e3)

-- Increase (by one) all indices above n in t
liftType :: _ => Int -> Type -> Type
liftType n (TVar x) = TVar $ mut LiftTypeTVar (if x < n then x else x + 1) (x + 1)
liftType n (ForAll x) = ForAll $ liftType (mut LiftTypeForAll (n + 1) n) x
liftType n (t1 :-> t2) = liftType n t1 :-> liftType n t2
liftType _ x = x

substInType :: _ => Int -> Type -> Type -> Type
substInType n s (TVar x)
  | x == n = s
  | mut SubstInTypeLT (x > n) (x < n) = TVar $ mut SubstInTypeNoDecr (x -1) x
  | otherwise = TVar x
substInType n s (ForAll e) = ForAll $ substInType (mut SubstInTypeNoIncr (n + 1) n) (mut SubstInTypeNoLift (liftType 0 s) s) e
--substInType n s (ForAll e)  = ForAll $ substInType (n+1) (mut SubstInTypeNoLift (liftType 0 s) s) e
substInType n s (t1 :-> t2) = substInType n s t1 :-> substInType n s t2
substInType _ _ x = x

tSubst :: _ => Int -> Type -> Expr -> Expr
tSubst n s (TLam e) = TLam $ tSubst (mut TSubstNoIncr (n + 1) n) (mut TSubstNoLift (liftType 0 s) s) e
tSubst n s (TApp e t) = TApp (tSubst n s e) (substInType n s t)
tSubst n s (Lam t e) = Lam (substInType n s t) (tSubst n s e)
tSubst n s (e1 :@: e2) = tSubst n s e1 :@: tSubst n s e2
tSubst n s (Cond e1 e2 e3) = Cond (tSubst n s e1) (tSubst n s e2) (tSubst n s e3)
tSubst _ _ x = x

------------------------------------------
-- PARALLEL REDUCTION
------------------------------------------

pstep :: _ => Expr -> Expr
pstep (Lam t e1) =
  let e1' = pstep e1
   in Lam t e1'
pstep (e1 :@: e2) =
  let e1' = pstep e1
   in let e2' = pstep e2
       in case e1' of
            Lam _ body -> mut AppForgetSubst (mut SubstSwapped (subst 0 e2' body) (subst 0 body e2')) body
            _ -> e1' :@: e2'
pstep (TLam e1) =
  let e1' = pstep e1
   in TLam e1'
pstep (TApp e1 t) =
  let e1' = pstep e1
   in case e1' of
        TLam body -> mut TAppForgetSubst (tSubst 0 t body) body
        _ -> TApp e1' t
pstep (Cond e1 e2 e3) =
  let e1' = pstep e1
   in let e2' = pstep e2
       in let e3' = pstep e3
           in case e1' of
                BTrue -> e2'
                BFalse -> mut CondFalseToTrue e3' e2'
                _ -> Cond e1' e2' e3'
pstep e = e

peval :: _ => Expr -> Expr
peval e =
  let e' = pstep e
   in if e' == e then e else peval e'

------------------------------------------
-- LAMBDA TERM UTILITIES
------------------------------------------

wellTyped :: Expr -> Bool
wellTyped = isJust . typeOf

size :: Expr -> Int
size Con = 1
size BTrue = 1
size BFalse = 1
size (Var _) = 1
size (Lam _ e) = 1 + size e
size (e1 :@: e2) = 1 + size e1 + size e2
size (Cond e1 e2 e3) = 1 + size e1 + size e2 + size e3
size (TApp e _) = 1 + size e
size (TLam e) = 1 + size e

------------------------------------------
-- GENERATION
------------------------------------------

forall :: Iso Type Type
forall =
  Iso
    (Just . ForAll)
    ( \case
        ForAll t -> Just t
        _ -> Nothing
    )

arrow :: Iso (Type, Type) Type
arrow =
  Iso
    (Just . uncurry (:->))
    ( \case
        t1 :-> t2 -> Just (t1, t2)
        _ -> Nothing
    )

lam :: Iso (Type, Expr) Expr
lam =
  Iso
    (Just . uncurry Lam)
    ( \case
        Lam t e -> Just (t, e)
        _ -> Nothing
    )

tlam :: Iso Expr Expr
tlam =
  Iso
    (Just . TLam)
    ( \case
        TLam e -> Just e
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

tapp :: Iso (Expr, Type) Expr
tapp =
  Iso
    (Just . uncurry TApp)
    ( \case
        TApp e t -> Just (e, t)
        _ -> Nothing
    )

genType :: Syntax d => Int -> d Type
genType freeTypeVar = arb freeTypeVar (10 :: Int) -- TODO Size
  where
    arb ftv 0 = uniform $ pure Base : fmap (pure . TVar) [0 .. ftv -1]
    arb ftv n =
      select
        "TYPE"
        [ arb ftv 0,
          arrow <$> (arb ftv (n `div` 6) <*> arb ftv (n `div` 4)),
          forall <$> arb (ftv + 1) (n -1)
        ]

genExpr :: Syntax d => d Expr
genExpr = depend typeOf <$> bind (genType 0) genExprOf

genExprOf :: forall d. Syntax d => Type -> d Expr
genExprOf ty = let ?mutant = NoMutant in arb 0 [] ty (10 :: Int) -- TODO Size
  where
    arb ftv c t 0 =
      uniform $
        [pure Con | t == Base]
          ++ [pure (Var i) | (i, t') <- zip [0 ..] c, t == t']
          ++ [lam <$> (pure t1 <*> arb ftv (t1 : c) t2 0) | t1 :-> t2 <- [t]]
          ++ [tlam <$> arb (ftv + 1) (map (let ?mutant = NoMutant in liftType 0) c) t1 0 | ForAll t1 <- [t]]
    arb ftv c t n =
      select
        "CONSTRUCTOR"
        [ arb ftv c t 0,
          uniform $
            [lam <$> (pure t1 <*> arb ftv (t1 : c) t2 (n - 1)) | t1 :-> t2 <- [t]]
              ++ [tlam <$> arb (ftv + 1) (map (let ?mutant = NoMutant in liftType 0) c) t1 (n - 1) | ForAll t1 <- [t]],
          depend (unGenApp ftv c)
            <$> bind
              (uniform (map pure . nub $ michal c t))
              (genApp ftv c t n),
          depend (unGenApp ftv c)
            <$> bind
              (genType ftv)
              (genApp ftv c t n),
          depend
            ( \case
                TApp e t' -> fmap (,t') (typeOf' ftv c e)
                _ -> Nothing
            )
            <$> bind
              (genT1T2 t)
              (\(t1, t2) -> tapp <$> (arb ftv c t1 (n - 1) <*> pure t2))
        ]

    genApp ftv c t n t2 = app <$> (arb ftv c (t2 :-> t) (n `div` 2) <*> arb ftv c t2 (n `div` 2))
    unGenApp ftv c = \case
      _ :@: e2 -> typeOf' ftv c e2
      _ -> Nothing

michal :: [Type] -> Type -> [Type]
michal c t =
  [ t' | varType <- c, t' <- aux varType
  ]
  where
    aux (t1 :-> t2)
      | t == t2 = [t1]
      | t /= t2 = aux t2
    aux _ = []

isClosed :: Type -> Bool
isClosed = isClosed' 0
  where
    isClosed' :: Int -> Type -> Bool
    isClosed' tc (TVar x) = x < tc
    isClosed' tc (t1 :-> t2) = isClosed' tc t1 && isClosed' tc t2
    isClosed' tc (ForAll t) = isClosed' (tc + 1) t
    isClosed' _ TBool = True
    isClosed' _ Base = True

-- Randomly fetch a subterm of a type
fetchSubType :: Syntax d => Type -> d Type
fetchSubType t =
  select
    "FETCH_SUB"
    [ uniform [pure t | isClosed t],
      uniform [fetchSubType t1 | t1 :-> _ <- [t]],
      uniform [fetchSubType t2 | _ :-> t2 <- [t]],
      uniform [fetchSubType t' | ForAll t' <- [t]]
    ]

-- "Replace (some occurrences of) closed type s in type t by (TVar n)"
replaceSubType :: Syntax d => Int -> Type -> Type -> d Type
replaceSubType n s t =
  select
    "REPLACE_SUB"
    [ pure t,
      uniform [pure (TVar n) | s == t],
      uniform [do arrow <$> (replaceSubType n s t1 <*> replaceSubType n s t2) | t1 :-> t2 <- [t]],
      uniform [do forall <$> replaceSubType (n + 1) s t' | ForAll t' <- [t], t' == s]
    ]

-- Generate t1 t2 such that t1{0:=t2} = t
genT1T2 :: Syntax d => Type -> d (Type, Type)
genT1T2 ty =
  let t' = let ?mutant = NoMutant in liftType 0 ty
   in depend (Just . snd)
        <$> bind (fetchSubType t') (\t2 -> (forall <$> replaceSubType 0 t2 t') <*> pure t2)

prop_genExprOfOK :: Property
prop_genExprOfOK =
  forAll (generate_ (genType 0)) $ \t ->
    forAll (runGenT . unMGen $ genExprOf t) $ \e ->
      isJust e ==> Just t == (typeOf =<< e)

prop_genExprOK :: Property
prop_genExprOK = forAll ((runGenT . unMGen) genExpr) $ \e -> isJust e ==> isJust (typeOf =<< e)

test :: IO ()
test = do
  quickCheck prop_genExprOfOK
  quickCheck prop_genExprOK