{-# LANGUAGE LambdaCase #-}

module ExprExample where

import ParserGen
import Prelude hiding (div, id, pure, sum, (*>), (.), (<$>), (<*), (<*>))

data Expr
  = Term Term
  | Plus Expr Term
  | Minus Expr Term
  deriving (Show)

data Term = Factor Factor | Times Term Factor | Div Term Factor
  deriving (Show)

data Factor = Digits Digits | Pos Factor | Neg Factor | Parens Expr
  deriving (Show)

data Digits = Digit Char | More Char Digits
  deriving (Show)

term :: Iso Term Expr
term =
  Iso
    (Just . Term)
    ( \case
        Term t -> Just t
        _ -> Nothing
    )

plus :: Iso (Expr, Term) Expr
plus =
  Iso
    (Just . uncurry Plus)
    ( \case
        Plus e t -> Just (e, t)
        _ -> Nothing
    )

minus :: Iso (Expr, Term) Expr
minus =
  Iso
    (Just . uncurry Minus)
    ( \case
        Minus e t -> Just (e, t)
        _ -> Nothing
    )

factor :: Iso Factor Term
factor =
  Iso
    (Just . Factor)
    ( \case
        Factor t -> Just t
        _ -> Nothing
    )

times :: Iso (Term, Factor) Term
times =
  Iso
    (Just . uncurry Times)
    ( \case
        Times t f -> Just (t, f)
        _ -> Nothing
    )

div :: Iso (Term, Factor) Term
div =
  Iso
    (Just . uncurry Div)
    ( \case
        Div t f -> Just (t, f)
        _ -> Nothing
    )

digits :: Iso Digits Factor
digits =
  Iso
    (Just . Digits)
    ( \case
        Digits d -> Just d
        _ -> Nothing
    )

pos :: Iso Factor Factor
pos =
  Iso
    (Just . Pos)
    ( \case
        Pos f -> Just f
        _ -> Nothing
    )

neg :: Iso Factor Factor
neg =
  Iso
    (Just . Neg)
    ( \case
        Neg f -> Just f
        _ -> Nothing
    )

parens :: Iso Expr Factor
parens =
  Iso
    (Just . Parens)
    ( \case
        Parens e -> Just e
        _ -> Nothing
    )

digit :: Iso Char Digits
digit =
  Iso
    (Just . Digit)
    ( \case
        Digit i -> Just i
        _ -> Nothing
    )

more :: Iso (Char, Digits) Digits
more =
  Iso
    (Just . uncurry More)
    ( \case
        More i d -> Just (i, d)
        _ -> Nothing
    )

genExpr :: Syntax d => Int -> d Expr
genExpr = \case
  0 -> term <$> genTerm 0
  n ->
    select
      "EXPR"
      [ term <$> genTerm (n - 1),
        plus <$> (genExpr (n - 1) <*> genTerm (n - 1)),
        minus <$> (genExpr (n - 1) <*> genTerm (n - 1))
      ]

genTerm :: Syntax d => Int -> d Term
genTerm = \case
  0 -> factor <$> genFactor 0
  n ->
    select
      "TERM"
      [ factor <$> genFactor (n - 1),
        times <$> (genTerm (n - 1) <*> genFactor (n - 1)),
        div <$> (genTerm (n - 1) <*> genFactor (n - 1))
      ]

genFactor :: Syntax d => Int -> d Factor
genFactor = \case
  0 -> digits <$> genDigits 0
  n ->
    select
      "FACTOR"
      [ digits <$> genDigits (n - 1),
        pos <$> genFactor (n - 1),
        neg <$> genFactor (n - 1),
        parens <$> genExpr (n - 1)
      ]

genDigits :: Syntax d => Int -> d Digits
genDigits = \case
  0 -> digit <$> int
  n ->
    select
      "DIGITS"
      [ digit <$> int,
        more <$> (int <*> genDigits (n - 1))
      ]
  where
    int = select "INT" (fmap pure ['0' .. '9'])

example :: Expr
example =
  Plus (Term (Factor (Digits (Digit '1')))) (Factor (Parens (Term (Times (Factor (Digits (Digit '2'))) (Digits (Digit '3'))))))