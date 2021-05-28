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

-- Tree example
leaf :: Iso () BST
leaf = Iso (\() -> Just Leaf) (\case Leaf -> Just (); _ -> Nothing)

node :: Iso ((BST, Int), BST) BST
node =
  Iso
    (\((l, x), r) -> Just (Node l x r))
    (\case Node l x r -> Just ((l, x), r); _ -> Nothing)

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
