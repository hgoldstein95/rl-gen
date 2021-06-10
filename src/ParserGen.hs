{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module ParserGen where

import BST (BST (..), prune)
import qualified BST
import Control.Arrow (Arrow (second))
import Control.Monad (MonadPlus (mplus), liftM2, (>=>))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.State (MonadState (get, put), StateT (..))
import Control.Monad.Trans (lift)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import QuickCheck.GenT (Gen, GenT, MonadGen (liftGen), frequency, runGenT, shuffle)
import qualified Test.QuickCheck as QC
import Prelude hiding (foldl, id, iterate, pure, sum, (*>), (.), (<$>), (<*), (<*>))
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

data Iso a b = Iso (a -> Maybe b) (b -> Maybe a) -- Can this be total?

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

ignore :: Eq a => a -> Iso a ()
ignore x = Iso f g
  where
    f y = if x == y then Just () else Nothing
    g () = Just x

subset :: (a -> Bool) -> Iso a a
subset p = Iso f f
  where
    f x
      | p x = Just x
      | otherwise = Nothing

nil :: Iso () [a]
nil = Iso (Just . const []) (\case [] -> Just (); _ -> Nothing)

cons :: Iso (a, [a]) [a]
cons = Iso (Just . uncurry (:)) (\case x : xs -> Just (x, xs); _ -> Nothing)

foldl :: Iso (a, b) a -> Iso (a, [b]) a
foldl i = inverse unit . (id `times` inverse nil) . iterate (step i)
  where
    step j = (j `times` id) . associate . (id `times` inverse cons)

times :: Iso a b -> Iso c d -> Iso (a, c) (b, d)
times i j = Iso f g
  where
    f (a, b) = liftM2 (,) (apply i a) (apply j b)
    g (c, d) = liftM2 (,) (unapply i c) (unapply j d)

driver :: (a -> Maybe a) -> a -> a
driver step state =
  case step state of
    Just state' -> driver step state'
    Nothing -> state

iterate :: Iso a a -> Iso a a
iterate step = Iso f g
  where
    f = Just . driver (apply step)
    g = Just . driver (unapply step)

depend :: (b -> Maybe a) -> Iso (a, b) b
depend f = Iso (\(_, b) -> Just b) (\b -> f b >>= \a -> Just (a, b))

class IsoFunctor f where
  (<$>) :: Iso a b -> (f a -> f b)

class ProductFunctor f where
  (<*>) :: f a -> f b -> f (a, b)

class (IsoFunctor d, ProductFunctor d) => Syntax d where
  pure :: Eq a => a -> d a
  bind :: d a -> (a -> d b) -> d (a, b) -- Name in CT?
  select :: String -> [d a] -> d a
  empty :: d a
  uniform :: [d a] -> d a

(*>) :: Syntax d => d () -> d a -> d a
p *> q = inverse unit . commute <$> (p <*> q)

(<*) :: Syntax d => d a -> d () -> d a
p <* q = inverse unit <$> (p <*> q)

instance IsoFunctor Printer where
  iso <$> Printer p = Printer (unapply iso >=> p)

instance ProductFunctor Printer where
  Printer p <*> Printer q = Printer (\(x, y) -> liftM2 (++) (p x) (q y))

emit :: (String, [Int]) -> Printer ()
emit p = ignore p <$> Printer (\t -> Just [t])

sum :: Foldable t => t (Printer a) -> Printer a
sum ps = Printer $ \x -> foldr (mplus . (($ x) . runPrinter)) Nothing ps

instance Syntax Printer where
  pure x = Printer (\y -> if x == y then Just [] else Nothing)
  select s ds =
    sum $
      zipWith
        (\i d -> emit (s, [if x == i then 1 else 0 | x <- [0 .. length ds - 1]]) *> d)
        [0 ..]
        ds
  bind (Printer p) f = Printer $ \(a, b) -> liftM2 (++) (p a) (runPrinter (f a) b)
  empty = Printer (const Nothing)
  uniform ds = sum ds

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
  bind (MGen ma) f = MGen $ do
    a <- ma
    b <- unMGen (f a)
    return (a, b)
  empty = MGen $ lift Nothing
  uniform = select ""

type Dist = Map String [Int]

freqShuffle :: MonadGen g => [(Int, a)] -> g [a]
freqShuffle gs = do
  idxs <- aux (zip (map fst gs) [0 ..])
  return $ map (snd . (gs !!)) idxs
  where
    aux [] = return []
    aux is = do
      i <- frequency (map (second return) is)
      fmap (i :) (aux (filter ((/= i) . snd) is))

newtype OGen a = OGen {unOGen :: ReaderT Dist (GenT Maybe) a}
  deriving (Functor, Monad, Applicative, MonadReader Dist)

instance IsoFunctor OGen where
  iso <$> g = g >>= \x -> OGen $ lift (lift (apply iso x))

instance ProductFunctor OGen where
  g1 <*> g2 = g1 >>= \x -> g2 >>= (\y -> return (x, y))

instance Syntax OGen where
  pure x = return x
  select s ds = do
    m <- ask
    OGen . lift $ lift =<< liftGen (aux m =<< freqShuffle (zip (Map.findWithDefault [1 ..] s m) [0 .. length ds - 1]))
    where
      aux m (i : is) = do
        runGenT (runReaderT (unOGen (ds !! i)) m) >>= \case
          Nothing -> aux m is
          Just x -> return (Just x)
      aux _ [] = return Nothing
  bind ma f = do
    a <- ma
    b <- f a
    return (a, b)
  empty = OGen $ lift (lift Nothing)
  uniform = select ""

-- | This is essentially a Q-table. It maps (ChoicePoint, State) -> Freqs
type ChoiceState = [(String, Int)]

type ChoiceTable = Map (String, ChoiceState) [Int]

newtype SGen a = SGen {unSGen :: StateT ChoiceState (ReaderT ChoiceTable (GenT Maybe)) a}
  deriving (Functor, Monad, Applicative, MonadReader ChoiceTable, MonadState ChoiceState)

instance IsoFunctor SGen where
  iso <$> g = g >>= \x -> SGen $ lift . lift . lift $ apply iso x

instance ProductFunctor SGen where
  g1 <*> g2 = g1 >>= \x -> g2 >>= (\y -> return (x, y))

liftMaybe :: Maybe a -> SGen a
liftMaybe = SGen . lift . lift . lift

instance Syntax SGen where
  pure x = return x
  select sid ds = do
    s <- get
    m <- ask
    let tryGenerators = \case
          (i : is) -> do
            runGenT (runReaderT (runStateT (unSGen (ds !! i)) s) m) >>= \case
              Nothing -> tryGenerators is
              Just (x, s') -> return (Just (x, s', i))
          [] -> return Nothing

    let is = Map.findWithDefault [1 ..] (sid, s) m
    (x, s', i) <-
      SGen . lift . lift $
        lift =<< liftGen (freqShuffle (zip is [0 .. length ds - 1]) >>= tryGenerators)
    put (s' ++ [(sid, i)])
    return x

  bind ma f = do
    a <- ma
    b <- f a
    return (a, b)
  empty = liftMaybe Nothing
  uniform = select ""

ungenerate :: Printer a -> a -> Dist
ungenerate p = Map.unionsWith (zipWith (+)) . map (uncurry Map.singleton) . fromJust . runPrinter p

generate :: MGen a -> IO a
generate = fmap fromJust . QC.generate . runGenT . unMGen

generate_ :: MGen a -> Gen a
generate_ = fmap fromJust . runGenT . unMGen

generateFreq :: Dist -> OGen a -> IO a
generateFreq m = fmap fromJust . QC.generate . runGenT . (`runReaderT` normalizeFreqs m) . unOGen
  where
    normalizeFreqs = Map.map (map (\i -> if i > 0 then i * 100 else 1)) -- Is this right?

invertFreqs :: [Int] -> [Int]
invertFreqs is =
  if 0 `elem` is
    then map (\i -> if i == 0 then 1 else 0) is
    else
      let m = maximum is
       in map ((round :: Double -> Int) . (* fromIntegral m) . (1.0 /) . fromIntegral) is

invertAll :: Dist -> Dist
invertAll = Map.map invertFreqs

invert :: String -> Dist -> Dist
invert = Map.update (Just . invertFreqs)

set :: String -> [Int] -> Dist -> Dist
set = Map.insert

-- Tree example
int :: Syntax d => d Int
int = select "INT" (fmap pure [0 .. 20])

leaf :: Iso () BST
leaf = Iso (\() -> Just Leaf) (\case Leaf -> Just (); _ -> Nothing)

node :: Iso ((BST, Int), BST) BST
node =
  Iso
    (\((l, x), r) -> Just (Node l x r))
    (\case Node l x r -> Just ((l, x), r); _ -> Nothing)

-- Works OK
tree :: Syntax d => d BST
tree = aux (30 :: Int)
  where
    aux 0 = leaf <$> pure ()
    aux n =
      select
        "BST"
        [ leaf <$> pure (),
          node <$> ((aux (n `div` 2) <*> int) <*> aux (n `div` 2))
        ]

-- Doesn't work very well
ints :: Syntax d => d [Int]
ints = aux (30 :: Int)
  where
    aux 0 = nil <$> pure ()
    aux n = select "INTS" [nil <$> pure (), cons <$> (int <*> aux (n - 1))]

insert :: Iso (Int, BST) BST
insert = Iso (Just . uncurry BST.insert) prune

bst :: Syntax d => d BST
bst = foldl (insert . commute) <$> (pure Leaf <*> ints)

interestingIdea :: IO BST
interestingIdea =
  let m = ungenerate bst (Node Leaf 2 (Node (Node Leaf 14 Leaf) 16 Leaf))
   in generateFreq (set "LIST" [1, 10] $ invert "INT" m) bst

foo :: Syntax d => d (Int, Int)
foo = bind int (\i -> if even i then select "A" [pure 1, pure 2] else select "B" [pure 3, pure 4])