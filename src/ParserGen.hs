{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module ParserGen where

import BST (BST (..))
import Control.Monad (MonadPlus (mplus), liftM2, (>=>))
import Control.Monad.Trans (lift)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import QuickCheck.GenT (GenT, MonadGen (liftGen), frequency, runGenT, shuffle)
import qualified Test.QuickCheck as QC
import Prelude hiding (id, pure, sum, (*>), (.), (<$>), (<*), (<*>))
import qualified Prelude

class Category cat where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
  id = Prelude.id
  (.) = \g f x -> g (f x)

instance Category Iso where
  id = Iso Just Just
  g . f = Iso (apply f >=> apply g) (unapply g >=> unapply f)

newtype Printer a = Printer {runPrinter :: a -> Maybe [(String, [Int])]}

data Iso a b = Iso (a -> Maybe b) (b -> Maybe a)

inverse :: Iso a b -> Iso b a
inverse (Iso f g) = Iso g f

apply :: Iso a b -> a -> Maybe b
apply (Iso f _) = f

unapply :: Iso a b -> b -> Maybe a
unapply (Iso _ g) = g

associate :: Iso (a, (b, c)) ((a, b), c)
associate = Iso f g
  where
    f (a, (b, c)) = Just ((a, b), c)
    g ((a, b), c) = Just (a, (b, c))

commute :: Iso (a, b) (b, a)
commute = Iso f f
  where
    f (a, b) = Just (b, a)

unit :: Iso a (a, ())
unit = Iso f g
  where
    f a = Just (a, ())
    g (a, ()) = Just a

ignore :: a -> Iso a ()
ignore x = Iso f g
  where
    f _ = Just ()
    g () = Just x

subset :: (a -> Bool) -> Iso a a
subset p = Iso f f
  where
    f x
      | p x = Just x
      | otherwise = Nothing

class IsoFunctor f where
  (<$>) :: Iso a b -> (f a -> f b)

class ProductFunctor f where
  (<*>) :: f a -> f b -> f (a, b)

class (IsoFunctor d, ProductFunctor d) => Syntax d where
  pure :: Eq a => a -> d a
  select :: String -> [d a] -> d a

(*>) :: Syntax d => d () -> d a -> d a
p *> q = inverse unit . commute <$> (p <*> q)

(<*) :: Syntax d => d a -> d () -> d a
p <* q = inverse unit <$> (p <*> q)

instance IsoFunctor Printer where
  iso <$> Printer p = Printer (unapply iso >=> p)

instance ProductFunctor Printer where
  Printer p <*> Printer q = Printer (\(x, y) -> liftM2 (++) (p x) (q y))

instance Syntax Printer where
  pure x = Printer (\y -> if x == y then Just [] else Nothing)
  select s ds = sum $ zipWith (\i d -> (ignore (s, [if x == i then 1 else 0 | x <- [0 .. length ds - 1]]) <$> Printer (\t -> Just [t])) *> d) [0 ..] ds
    where
      sum ps = Printer $ \x -> foldr (mplus . (($ x) . runPrinter)) Nothing ps

newtype MGen a = MGen {unMGen :: GenT Maybe a}

instance IsoFunctor MGen where
  iso <$> MGen g = MGen $ g >>= \x -> lift (apply iso x)

instance ProductFunctor MGen where
  MGen g1 <*> MGen g2 = MGen $ g1 >>= \x -> g2 >>= \y -> return (x, y)

instance Syntax MGen where
  pure = MGen . return
  select _ ds = MGen $ lift =<< liftGen (aux =<< shuffle [0 .. length ds - 1])
    where
      aux (i : is) = do
        runGenT (unMGen (ds !! i)) >>= \case
          Nothing -> aux is
          Just x -> return (Just x)
      aux [] = return Nothing

type Dist = Map String [Int]

newtype OGen a = OGen {unOGen :: Dist -> GenT Maybe a}

instance IsoFunctor OGen where
  iso <$> OGen g = OGen $ g >=> \x -> lift (apply iso x)

instance ProductFunctor OGen where
  OGen g1 <*> OGen g2 = OGen $ \d -> g1 d >>= \x -> g2 d >>= \y -> return (x, y)

-- -- | TODO: Should be able to intermix frequency levels
-- freqShuffle :: MonadGen g => [(Int, a)] -> g [a]
-- freqShuffle =
--   fmap concat
--     . mapM (shuffle . map snd)
--     . groupBy (\x y -> fst x == fst y)
--     . reverse
--     . sortBy (comparing fst)
--     . filter ((> 0) . fst)

-- select s ds = OGen $ \m -> lift =<< liftGen (aux m =<< freqShuffle (zip (m ! s) [0 .. length ds - 1]))
--   where
--     aux m (i : is) = do
--       runGenT (unOGen (ds !! i) m) >>= \case
--         Nothing -> aux m is
--         Just x -> return (Just x)
--     aux _ [] = return Nothing

instance Syntax OGen where
  pure x = OGen $ \_ -> return x
  select s ds = OGen $ \m -> frequency $ zip (m ! s) (fmap (($ m) . unOGen) ds)

leaf :: Iso () BST
leaf =
  Iso
    (\() -> Just Leaf)
    ( \case
        Leaf -> Just ()
        Node {} -> Nothing
    )

node :: Iso ((BST, Int), BST) BST
node =
  Iso
    (\((l, x), r) -> Just (Node l x r))
    ( \case
        Leaf -> Nothing
        Node l x r -> Just ((l, x), r)
    )

tree :: Syntax d => d BST
tree = aux (30 :: Int)
  where
    aux 0 = leaf <$> pure ()
    aux n =
      select
        "NODE"
        [ leaf <$> pure (),
          node <$> ((aux (n `div` 2) <*> int) <*> aux (n `div` 2))
        ]
    int = select "INT" (fmap pure [0 .. 10])

ungenerate :: Printer a -> a -> Dist
ungenerate p = Map.unionsWith (zipWith (+)) . map (uncurry Map.singleton) . fromJust . runPrinter p

generate :: MGen a -> IO a
generate = fmap fromJust . QC.generate . runGenT . unMGen

generateFreq :: Dist -> OGen a -> IO a
generateFreq m = fmap fromJust . QC.generate . runGenT . ($ m) . unOGen

invert :: Dist -> Dist
invert =
  Map.map
    ( \is ->
        if 0 `elem` is
          then map (\i -> if i == 0 then 1 else 0) is
          else
            let m = maximum is
             in map ((round :: Double -> Int) . (* fromIntegral m) . (1.0 /) . fromIntegral) is
    )

data Expr
  = Term Term
  | Plus Expr Term
  | Minus Expr Term
  deriving (Show)

isoTerm :: Iso Term Expr
isoTerm =
  Iso
    (Just . Term)
    ( \case
        Term t -> Just t
        _ -> Nothing
    )

isoPlus :: Iso (Expr, Term) Expr
isoPlus =
  Iso
    (Just . uncurry Plus)
    ( \case
        Plus e t -> Just (e, t)
        _ -> Nothing
    )

isoMinus :: Iso (Expr, Term) Expr
isoMinus =
  Iso
    (Just . uncurry Minus)
    ( \case
        Minus e t -> Just (e, t)
        _ -> Nothing
    )

data Term = Factor Factor | Times Term Factor | Div Term Factor
  deriving (Show)

isoFactor :: Iso Factor Term
isoFactor =
  Iso
    (Just . Factor)
    ( \case
        Factor t -> Just t
        _ -> Nothing
    )

isoTimes :: Iso (Term, Factor) Term
isoTimes =
  Iso
    (Just . uncurry Times)
    ( \case
        Times t f -> Just (t, f)
        _ -> Nothing
    )

isoDiv :: Iso (Term, Factor) Term
isoDiv =
  Iso
    (Just . uncurry Div)
    ( \case
        Div t f -> Just (t, f)
        _ -> Nothing
    )

data Factor = Digits Digits | Pos Factor | Neg Factor | Parens Expr
  deriving (Show)

isoDigits :: Iso Digits Factor
isoDigits =
  Iso
    (Just . Digits)
    ( \case
        Digits d -> Just d
        _ -> Nothing
    )

isoPos :: Iso Factor Factor
isoPos =
  Iso
    (Just . Pos)
    ( \case
        Pos f -> Just f
        _ -> Nothing
    )

isoNeg :: Iso Factor Factor
isoNeg =
  Iso
    (Just . Neg)
    ( \case
        Neg f -> Just f
        _ -> Nothing
    )

isoParens :: Iso Expr Factor
isoParens =
  Iso
    (Just . Parens)
    ( \case
        Parens e -> Just e
        _ -> Nothing
    )

data Digits = Digit Int | More Int Digits
  deriving (Show)

isoDigit :: Iso Int Digits
isoDigit =
  Iso
    (Just . Digit)
    ( \case
        Digit i -> Just i
        _ -> Nothing
    )

isoMore :: Iso (Int, Digits) Digits
isoMore =
  Iso
    (Just . uncurry More)
    ( \case
        More i d -> Just (i, d)
        _ -> Nothing
    )

expr :: Syntax d => Int -> d Expr
expr = \case
  0 -> isoTerm <$> term 0
  n ->
    select
      "EXPR"
      [ isoTerm <$> term (n - 1),
        isoPlus <$> (expr (n - 1) <*> term (n - 1)),
        isoMinus <$> (expr (n - 1) <*> term (n - 1))
      ]

term :: Syntax d => Int -> d Term
term = \case
  0 -> isoFactor <$> factor 0
  n ->
    select
      "TERM"
      [ isoFactor <$> factor (n - 1),
        isoTimes <$> (term (n - 1) <*> factor (n - 1)),
        isoDiv <$> (term (n - 1) <*> factor (n - 1))
      ]

factor :: Syntax d => Int -> d Factor
factor = \case
  0 -> isoDigits <$> digits 0
  n ->
    select
      "FACTOR"
      [ isoDigits <$> digits (n - 1),
        isoPos <$> factor (n - 1),
        isoNeg <$> factor (n - 1),
        isoParens <$> expr (n - 1)
      ]

digits :: Syntax d => Int -> d Digits
digits = \case
  0 -> isoDigit <$> int
  n ->
    select
      "DIGITS"
      [ isoDigit <$> int,
        isoMore <$> (int <*> digits (n - 1))
      ]
  where
    int = select "INT" (fmap pure [0 .. 9])

example :: Expr
example = Plus (Term (Factor (Digits (Digit 1)))) (Factor (Parens (Term (Times (Factor (Digits (Digit 2))) (Digits (Digit 3))))))