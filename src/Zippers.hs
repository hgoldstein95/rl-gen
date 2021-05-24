{-# LANGUAGE TupleSections #-}

module Zippers where

data BST = Leaf | Node BST Int BST
  deriving (Show)

data Direction = L | R
  deriving (Show)

aTree :: BST
aTree = Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) 4 (Node (Node Leaf 5 Leaf) 6 (Node Leaf 7 Leaf))

change :: [Direction] -> Int -> BST -> BST
change (L : ds) newX (Node l x r) = Node (change ds newX l) x r
change (R : ds) newX (Node l x r) = Node l x (change ds newX r)
change [] newX (Node l _ r) = Node l newX r
change _ _ Leaf = error "bad directions"

elemAt :: [Direction] -> BST -> Int
elemAt (L : ds) (Node l _ _) = elemAt ds l
elemAt (R : ds) (Node _ _ r) = elemAt ds r
elemAt [] (Node _ x _) = x
elemAt _ Leaf = error "bad directions"

(-:) :: a -> (a -> b) -> b
x -: f = f x

data Crumb = LeftCrumb Int BST | RightCrumb Int BST
  deriving (Show)

type Breadcrumbs = [Crumb]

type Zipper = (BST, Breadcrumbs)

goLeft :: Zipper -> Zipper
goLeft (Node l x r, bs) = (l, LeftCrumb x r : bs)
goLeft _ = error "can't go left"

goRight :: Zipper -> Zipper
goRight (Node l x r, bs) = (r, RightCrumb x l : bs)
goRight _ = error "can't go right"

goUp :: Zipper -> Zipper
goUp (l, LeftCrumb x r : bs) = (Node l x r, bs)
goUp (r, RightCrumb x l : bs) = (Node l x r, bs)
goUp _ = error "can't go up"

toBST :: Zipper -> BST
toBST (t, []) = t
toBST z = toBST (goUp z)

toZipper :: BST -> Zipper
toZipper = (,[])

modify :: (Int -> Int) -> Zipper -> Zipper
modify f (Node l x r, bs) = (Node l (f x) r, bs)
modify _ (Leaf, bs) = (Leaf, bs)

attach :: BST -> Zipper -> Zipper
attach t (_, bs) = (t, bs)
