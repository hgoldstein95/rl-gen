{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module IsoCat where

import Control.Monad (liftM2, (>=>))
import Control.Monad.Trans (lift)
import QuickCheck.GenT (GenT)
import Prelude hiding (Functor (..), id, (.))

class Category cat where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c

instance Category (->) where
  id a = a
  (g . f) a = g (f a)

newtype a ~> b = KM {unKM :: a -> Maybe b}

instance Category (~>) where
  id = KM Just
  (KM g) . (KM f) = KM (f >=> g)

data Iso cat a b = Iso (cat a b) (cat b a)

type (<~>) = Iso (~>)

instance Category (<~>) where
  id = Iso (KM Just) (KM Just)
  (Iso (KM f) (KM f')) . (Iso (KM g) (KM g')) = Iso (KM (g >=> f)) (KM (f' >=> g'))

class (Category c, Category d) => Functor c d f where
  fmap :: c a b -> d (f a) (f b)

(<$>) :: (Category c, Functor c (->) f) => c a b -> f a -> f b
(<$>) = fmap

class Functor (<~>) (->) m => IsoMonad m where
  pure :: Eq a => a -> m a
  bind :: m a -> (a -> m b) -> (b ~> a) -> m b

newtype Gen a = Gen {runGen :: GenT Maybe a}

instance Functor (<~>) (->) Gen where
  fmap (Iso (KM f) _) ma = Gen $ runGen ma >>= lift . f

instance IsoMonad Gen where
  pure = Gen . return
  bind (Gen ma) f _ = Gen $ do
    a <- ma
    runGen (f a)

newtype Ungen a = Ungen {runUngen :: a -> Maybe [(String, Int)]}

instance Functor (<~>) (->) Ungen where
  fmap (Iso _ (KM g)) (Ungen pa) = Ungen (g >=> pa)

instance IsoMonad Ungen where
  pure x = Ungen $ \y -> if x == y then Just [] else Nothing
  bind (Ungen p) f (KM g) =
    Ungen $ \b -> g b >>= \a -> liftM2 (++) (p a) (runUngen (f a) b)
