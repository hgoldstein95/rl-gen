module LogicGen where

import BST (BST (..))
import Control.Monad (ap)
import QuickCheck.GenT (MonadGen (..))
import Test.QuickCheck (Gen, getSize, oneof)
import qualified Test.QuickCheck as QC

newtype LGen a = LGen {unLGen :: [Gen a]}

instance Functor LGen where
  fmap f = LGen . fmap (fmap f) . unLGen

instance Applicative LGen where
  pure = LGen . pure . pure
  (<*>) = ap

instance Monad LGen where
  return = pure
  (LGen mx) >>= f =
    LGen . fmap (>>= (oneof . unLGen . f)) $ mx

instance MonadGen LGen where
  liftGen = LGen . (: [])
  variant n = LGen . fmap (variant n) . unLGen
  sized f = LGen . (: []) $ do
    n <- getSize
    let LGen gs = f n
    oneof gs
  resize n = LGen . fmap (resize n) . unLGen
  choose r = LGen [choose r]

-- | Fair conjunction.
-- | `x +++ y +++ z == oneof [x, y, z]`
(+++) :: LGen a -> LGen a -> LGen a
LGen x +++ LGen y = LGen (x ++ y)

genTree :: LGen BST
genTree = sized aux
  where
    aux 0 = pure Leaf
    aux n =
      pure Leaf
        +++ ( Node
                <$> aux (n `div` 2)
                <*> (pure 1 +++ pure 2 +++ pure 3)
                <*> aux (n `div` 2)
            )

generate :: LGen a -> IO a
generate = QC.generate . oneof . unLGen
