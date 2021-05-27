{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module ParserGen where

import BST (BST (..))
import Control.Monad (MonadPlus (mplus), liftM2, (>=>))
import Control.Monad.Trans (lift)
import Data.Maybe (fromJust)
import QuickCheck.GenT (GenT, MonadGen (choose, liftGen), oneof, runGenT)
import qualified Test.QuickCheck as QC
import Prelude hiding (id, pure, (*>), (.), (<$>), (<*), (<*>))
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

newtype Printer a = Printer {runPrinter :: a -> Maybe [(String, Int)]}

ungenerate :: Printer a -> a -> [(String, Int)]
ungenerate p = fromJust . runPrinter p

data Iso a b = Iso (a -> Maybe b) (b -> Maybe a)

class IsoFunctor f where
  (<$>) :: Iso a b -> (f a -> f b)

class ProductFunctor f where
  (<*>) :: f a -> f b -> f (a, b)

class Alternative f where
  (<|>) :: f a -> f a -> f a
  empty :: f a

asum :: Alternative f => [f a] -> f a
asum = foldr (<|>) empty

class (IsoFunctor d, ProductFunctor d, Alternative d) => Syntax d where
  pure :: Eq a => a -> d a
  token :: (String, Int) -> d (String, Int)

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

(*>) :: Syntax d => d () -> d a -> d a
p *> q = inverse unit . commute <$> (p <*> q)

(<*) :: Syntax d => d a -> d () -> d a
p <* q = inverse unit <$> (p <*> q)

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

instance IsoFunctor Printer where
  iso <$> Printer p = Printer (unapply iso >=> p)

instance ProductFunctor Printer where
  Printer p <*> Printer q = Printer (\(x, y) -> liftM2 (++) (p x) (q y))

instance Alternative Printer where
  Printer p <|> Printer q = Printer (\x -> mplus (p x) (q x))
  empty = Printer (const Nothing)

instance Syntax Printer where
  pure x = Printer (\y -> if x == y then Just [] else Nothing)
  token _ = Printer (\t -> Just [t])

newtype MGen a = MGen {unMGen :: GenT Maybe a}

instance IsoFunctor MGen where
  iso <$> MGen g = MGen $ g >>= \x -> lift (apply iso x)

instance ProductFunctor MGen where
  MGen g1 <*> MGen g2 = MGen $ g1 >>= \x -> g2 >>= \y -> return (x, y)

instance Alternative MGen where
  MGen g1 <|> MGen g2 =
    MGen $
      lift
        =<< liftGen
          ( oneof
              [ do
                  runGenT g1 >>= \case
                    Nothing -> runGenT g2
                    Just x -> return (Just x),
                do
                  runGenT g2 >>= \case
                    Nothing -> runGenT g1
                    Just x -> return (Just x)
              ]
          )
  empty = MGen $ lift Nothing

instance Syntax MGen where
  pure = MGen . return
  token (s, r) = MGen $ fmap (s,) (choose (0, r))

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

select :: Syntax d => String -> [d a] -> d a
select s ds = asum . zipWith (\n d -> (ignore (s, n) <$> token (s, length ds)) *> d) [0 ..] $ ds

tree :: Syntax d => d BST
tree =
  select
    "NODE"
    [ pure Leaf,
      node <$> ((tree <*> int) <*> tree)
    ]
  where
    int = select "VAL" (fmap pure [1 .. 10])

generate :: MGen a -> IO a
generate = fmap fromJust . QC.generate . runGenT . unMGen