{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module QuickCheckEnum where

import Control.Applicative (Alternative (..), empty, (<|>))
import Control.Monad (msum)
import Control.Monad.Random
  ( MonadPlus,
    MonadRandom (..),
    Random (random, randomR, randoms),
    RandomGen (split),
    StdGen,
    ap,
    join,
    liftM2,
    liftM3,
    liftM4,
    newStdGen,
  )
import Data.Function (on)
import Data.List (sortBy, transpose)
import qualified Data.Set as Set
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

infixr 0 ==>

infix 1 `classify`

--------------------------------------------------------------------
-- Generator

{- begin Hybrid -}
newtype Gen a = Gen {run :: Int -> StdGen -> [a]}

{- end Hybrid -}

sized :: (Int -> Gen a) -> Gen a
sized fgen = Gen (\n r -> run (fgen n) n r)

resize :: Int -> Gen a -> Gen a
resize n g = Gen (\_ r -> run g n r)

rand :: Gen StdGen
rand = Gen (\_ r -> [r])

-- TODO: Oneof
{- begin promote -}
promote :: (a -> Gen b) -> Gen (a -> b)
promote f = Gen (\n r -> [\a -> head $ run (f a) n r])

{- end promote -}

variant :: Int -> Gen a -> Gen a
variant v g = Gen (\n r -> run g n (rands r !! (v + 1)))
  where
    rands r0 = r1 : rands r2 where (r1, r2) = split r0

generate :: Int -> StdGen -> Gen a -> a
generate n rnd g = head $ run g size rnd'
  where
    (size, rnd') = randomR (0, n) rnd

generateSafe :: Int -> StdGen -> Gen a -> Maybe a
generateSafe n rnd g =
  case run g size rnd' of
    [] -> Nothing
    x : _ -> Just x
  where
    (size, rnd') = randomR (0, n) rnd

generate' :: Int -> StdGen -> Gen a -> [a]
generate' n rnd g = run g size rnd'
  where
    (size, rnd') = randomR (0, n) rnd

instance Functor Gen where
  fmap f m = m >>= return . f

instance Alternative Gen where
  empty = Gen $ \_ _ -> []
  ma <|> mb = Gen $ \i g -> run ma i g ++ run mb i g

instance MonadPlus Gen

{- begin HybridMonad -}
instance Monad Gen where
  return a = Gen (\_ _ -> [a])

  g >>= k =
    Gen
      ( \n r0 ->
          let (r1, r2) = split r0
              aux _ [] = []
              aux r (a : as) =
                let (r1', r2') = split r
                 in run (k a) n r1' ++ aux r2' as
           in aux r2 (run g n r1)
      )

{- end HybridMonad -}

{- begin mix -}
mix :: Gen a -> Gen a -> Gen a
mix g1 g2 =
  Gen
    ( \n r0 ->
        let (switch, r1) = random r0
         in if switch then run g1 n r1 else run g2 n r1
    )

{- end mix -}

{- begin enumerate -}
enumerate :: [a] -> Gen a
enumerate xs = Gen (\_ _ -> xs)

{- end enumerate -}

{- begin takeG -}
takeG :: Int -> Gen a -> Gen a
takeG n g = Gen (\s r -> take n $ run g s r)

{- end takeG -}

{- begin liftListFn -}
liftListFn :: ([a] -> [a]) -> Gen a -> Gen a
liftListFn f g = Gen (\s r -> f $ run g s r)

{- end liftListFn -}

nubOrdG :: Ord a => Gen a -> Gen a
nubOrdG = liftListFn $ nubOrd Set.empty
  where
    nubOrd _ [] = []
    nubOrd s (x : xs) = if Set.member x s then nubOrd s xs else x : nubOrd (Set.insert x s) xs

{- begin cut -}
cut :: Gen a -> Gen a
cut = takeG 1

{- end cut -}

{- begin filterG -}
filterG :: (a -> Bool) -> Gen a -> Gen a
filterG p = liftListFn (filter p) -- Gen (\s r -> filter pred $ run g s r)
{- end filterG -}

-- derived
choose :: Random a => (a, a) -> Gen a
choose bounds = (fst . randomR bounds) `fmap` rand

{- begin elements -}
elements :: [a] -> Gen a
elements [] = empty
-- Used to be: elements [] = error "QuickCheck.elements used with empty list"
elements xs = (xs !!) `fmap` choose (0, length xs - 1)

{- end elements -}

-- At each step pick from g1 and g2 randomly according to weight.
{- begin weightedChoice -}
weightedChoice :: (Int, Gen a) -> (Int, Gen a) -> Gen a
weightedChoice (w1, g1) (w2, g2) =
  Gen
    ( \s r0 ->
        let (r1, r2) = split r0
            (r1', r2') = split r1
         in weightedInterleave w1 w2 r2 (run g1 s r1') (run g2 s r2')
    )

{- end weightedChoice -}

{- begin weightedInterleave -}
weightedInterleave :: Int -> Int -> StdGen -> [a] -> [a] -> [a]
weightedInterleave _ _ _ l1 [] = l1
weightedInterleave _ _ _ [] l2 = l2
weightedInterleave w1 w2 r l1@(x : xs) l2@(y : ys) =
  let (switch, r2) = randomR (1, w1 + w2) r
   in if switch <= w1
        then x : weightedInterleave w1 w2 r2 xs l2
        else y : weightedInterleave w1 w2 r2 l1 ys

{- end weightedInterleave -}

{- begin randomize' -}
randomize' :: Gen a -> Gen a
randomize' g =
  Gen
    ( \n r0 ->
        let (r1, r2) = split r0
            ns = randoms r1 :: [Int]
            as = run g n r2
            ordered = sortBy (compare `on` fst) $ zip ns as
         in map snd ordered
    )

{- end randomize' -}

{- begin randomOrder' -}
randomOrder' :: [a] -> Gen a
randomOrder' = randomize' . enumerate

{- end randomOrder' -}

{- begin permuteWeighted -}
{-
permuteWeighted :: [(Int, a)] -> Gen [a]
permuteWeighted x =
  let Just u = Urn.fromList $ map (first fromIntegral) x
      aux u = do (w, a, mu) <- Urn.remove u
                 case mu of
                   Just u' -> (a:) <$> aux u'
                   _       -> pure [a]
  in aux u
-}
{- end permuteWeighted -}

{- begin randomize -}
{-
randomize :: Gen a -> Gen a
randomize g =
  Gen (\n r0 ->
         let (r1, r2) = split r0
             weightedValues = map (1, ) $ run g n r1
         in concat $ run (permuteWeighted weightedValues) n r2)
-}
-- De-urned
randomize :: Gen a -> Gen a
randomize g =
  Gen
    ( \n r0 ->
        let (r1, r2) = split r0
            ns = randoms r1 :: [Int]
            as = run g n r2
            ordered = sortBy (compare `on` fst) $ zip ns as
         in map snd ordered
    )

{- end randomize -}

instance MonadRandom Gen where
  getRandomR = choose

--instance Urn.MonadSample Gen

{- begin randomOrderOld -}
randomOrderOld :: [a] -> Gen a
randomOrderOld = randomize . enumerate

{- end randomOrderOld -}

{-
randomOrder'old :: [a] -> Gen a
randomOrder'old xs = do
  xs' <- permuteWeighted $ map (1,) xs
  enumerate xs'
-}

randomOrderN :: Int -> (Int, Int) -> Gen Int
randomOrderN m (l, h) = aux m Set.empty
  where
    aux 0 _ = empty
    aux n s = do
      mx <- choose (l, h) `suchThatMaybe` (\x -> not $ Set.member x s)
      case mx of
        Just x -> pure x <|> aux (n -1) (Set.insert x s)
        Nothing -> empty

randomOrder :: (Int, Int) -> Gen Int
randomOrder (l, h) = randomOrderN 10 (l, h)

-- This is just alternateM or something? mplus?
randomOrderN' :: Int -> (Int, Int) -> Gen Int
randomOrderN' n (l, h) = aux n
  where
    aux 0 = empty
    aux _ = choose (l, h) <|> aux (n -1)

-- Pick and drop a specific item from a list with no weights.
pickDropUnbiased :: a -> [a] -> Int -> (a, [a])
pickDropUnbiased def xs n =
  case xs of
    [] -> (def, [])
    (x : _) ->
      if n <= 0
        then (x, xs)
        else
          let (x', xs') = pickDropUnbiased def xs (n -1)
           in (x', x : xs')

{- begin shuffleGens -}
shuffleGens :: [Gen a] -> Gen a
shuffleGens = randomize . allof

{- end shuffleGens -}

{- begin shuffleGens' -}
shuffleGens' :: [Gen a] -> Gen a
shuffleGens' = randomize' . allof

{- end shuffleGens' -}

-- Not sure how performance compares. I'd think this is worse, but I recall it was performing better.
shuffleGensOld :: [Gen a] -> Gen a
shuffleGensOld gens = shuffle' (length gens) gens
  where
    shuffle' :: Int -> [Gen a] -> Gen a
    shuffle' 0 _ = empty
    shuffle' _ [] = empty
    shuffle' n gs =
      do
        let n' = n - 1
        i <- choose (0, length gs - 1)
        let (g, gs') = pickDropUnbiased empty gs i
        g <|> shuffle' n' gs'

{- begin allof -}
allof :: [Gen a] -> Gen a
allof = msum

{- end allof -}

{- begin fairallof -}
fairAllof :: [Gen a] -> Gen a
fairAllof gs =
  Gen
    ( \s r ->
        let seeds = randomseeds r
         in concat . transpose $ run <$> gs <*> repeat s <*> seeds
    )

{- end fairallof -}

randomseeds :: StdGen -> [StdGen]
randomseeds r = let (r1, r2) = split r in r1 : randomseeds r2

{- begin tryMany -}
tryMany :: Int -> Gen a -> Gen a
tryMany n g = allof $ replicate n g

{- end tryMany -}

{- begin retry -}
retry :: Int -> Gen a -> Gen a
retry n g = cut $ tryMany n g

{- end retry -}

vector :: Arbitrary a => Int -> Gen [a]
vector n = sequence [arbitrary | _ <- [1 .. n]]

vectorOf :: Int -> Gen a -> Gen [a]
vectorOf n g = sequence [g | _ <- [1 .. n]]

oneof :: [Gen a] -> Gen a
oneof gens = join (elements gens)

frequency :: [(Int, Gen a)] -> Gen a
frequency xs = choose (1, tot) >>= (`pick` xs)
  where
    tot = sum (map fst xs)

    pick _ [] = empty
    pick n ((k, x) : _)
      | n <= k = x
      | otherwise = pick (n - k) xs

-- general monadic

two :: Monad m => m a -> m (a, a)
two m = liftM2 (,) m m

three :: Monad m => m a -> m (a, a, a)
three m = liftM3 (,,) m m m

four :: Monad m => m a -> m (a, a, a, a)
four m = liftM4 (,,,) m m m m

--------------------------------------------------------------------
-- Arbitrary

class Arbitrary a where
  arbitrary :: Gen a
  coarbitrary :: a -> Gen b -> Gen b

instance Arbitrary () where
  arbitrary = return ()
  coarbitrary _ = variant 0

instance Arbitrary Bool where
  arbitrary = elements [True, False]
  coarbitrary b = if b then variant 0 else variant 1

instance Arbitrary Int where
  arbitrary = sized $ \n -> choose (- n, n)
  coarbitrary n = variant (if n >= 0 then 2 * n else 2 * (- n) + 1)

instance Arbitrary Integer where
  arbitrary = sized $ \n -> choose (- fromIntegral n, fromIntegral n)
  coarbitrary n = variant (fromInteger (if n >= 0 then 2 * n else 2 * (- n) + 1))

instance Arbitrary Float where
  arbitrary = liftM3 fraction arbitrary arbitrary arbitrary
  coarbitrary x = coarbitrary (decodeFloat x)

instance Arbitrary Double where
  arbitrary = liftM3 fraction arbitrary arbitrary arbitrary
  coarbitrary x = coarbitrary (decodeFloat x)

fraction :: Fractional a => Integer -> Integer -> Integer -> a
fraction a b c = fromInteger a + (fromInteger b / (abs (fromInteger c) + 1))

instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
  arbitrary = liftM2 (,) arbitrary arbitrary
  coarbitrary (a, b) = coarbitrary a . coarbitrary b

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a, b, c) where
  arbitrary = liftM3 (,,) arbitrary arbitrary arbitrary
  coarbitrary (a, b, c) = coarbitrary a . coarbitrary b . coarbitrary c

instance
  (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
  Arbitrary (a, b, c, d)
  where
  arbitrary = liftM4 (,,,) arbitrary arbitrary arbitrary arbitrary
  coarbitrary (a, b, c, d) =
    coarbitrary a . coarbitrary b . coarbitrary c . coarbitrary d

instance Arbitrary a => Arbitrary [a] where
  arbitrary = sized (\n -> choose (0, n) >>= vector)
  coarbitrary [] = variant 0
  coarbitrary (a : as) = coarbitrary a . variant 1 . coarbitrary as

instance (Arbitrary a, Arbitrary b) => Arbitrary (a -> b) where
  arbitrary = promote (`coarbitrary` arbitrary)
  coarbitrary f gen = arbitrary >>= ((`coarbitrary` gen) . f)

--------------------------------------------------------------------
-- Testable

data Result = Result {ok :: Maybe Bool, stamp :: [String], arguments :: [String]}

instance Show Result where
  show (Result k s args) = "Result: " ++ show k ++ " , " ++ show s ++ " , " ++ show args

nothing :: Result
nothing = Result {ok = Nothing, stamp = [], arguments = []}

newtype Property
  = Prop (Gen Result)

result :: Result -> Property
result res = Prop (return res)

evaluate :: Testable a => a -> Gen Result
evaluate a = gen where Prop gen = property a

class Testable a where
  property :: a -> Property

instance Testable () where
  property _ = result nothing

instance Testable Bool where
  property b = result (nothing {ok = Just b})

instance Testable (Either String ()) where
  property (Left _) = property False
  property (Right _) = property True

instance Testable Result where
  property res = result res

instance Testable Property where
  property prop = prop

instance (Arbitrary a, Show a, Testable b) => Testable (a -> b) where
  property f = forAll arbitrary f

forAll :: (Show a, Testable b) => Gen a -> (a -> b) -> Property
forAll gen body = Prop $
  do
    a <- gen
    res <- evaluate (body a)
    return (argument a res)
  where
    argument a res = res {arguments = show a : arguments res}

(==>) :: Testable a => Bool -> a -> Property
True ==> a = property a
False ==> _ = property ()

label :: Testable a => String -> a -> Property
label s a = Prop (add `fmap` evaluate a)
  where
    add res = res {stamp = s : stamp res}

classify :: Testable a => Bool -> String -> a -> Property
classify True name = label name
classify False _ = property

trivial :: Testable a => Bool -> a -> Property
trivial = (`classify` "trivial")

collect :: (Show a, Testable b) => a -> b -> Property
collect v = label (show v)

--
instance Applicative Gen where
  pure = return
  (<*>) = ap

--------------------------------------------------------------------
-- Testing

data Config = Config
  { configMaxTest :: Int,
    configMaxFail :: Int,
    configSize :: Int -> Int,
    configEvery :: Int -> [String] -> String
  }

quick :: Config
quick =
  Config
    { configMaxTest = 100,
      configMaxFail = 1000,
      configSize = (+ 3) . (`div` 2),
      configEvery = \n _ -> let s = show n in s ++ ['\b' | _ <- s]
    }

verbose :: Config
verbose =
  quick
    { configEvery = \n args -> show n ++ ":\n" ++ unlines args
    }

defaultConfig :: Config
defaultConfig = quick

data Summary = Summary {summaryOk :: Bool, summaryTests :: Int, summaryTime :: Integer, summaryFail :: Int}
  deriving (Show)

sample :: Show a => Gen a -> IO ()
sample a =
  do
    rnd <- newStdGen
    print $ generate' 10 rnd a

sampleList :: Show a => Gen a -> IO [a]
sampleList a =
  do
    rnd <- newStdGen
    return $ generate' 42 rnd a

sampleOne :: Show a => Gen a -> IO a
sampleOne a =
  do
    rnd <- newStdGen
    return $ generate 42 rnd a

sampleOneSafe :: Show a => Gen a -> IO (Maybe a)
sampleOneSafe a =
  do
    rnd <- newStdGen
    return $ generateSafe 42 rnd a

-- Not working yet:
within :: Testable a => Int -> a -> Property
within microseconds a =
  Prop $
    let Prop (Gen g) = property a
     in Gen $ \n r -> unsafePerformIO $ do
          res <- timeout microseconds $ return $! g n r
          case res of
            Just x -> return x
            Nothing -> return [nothing {ok = Just False}]

--  where aux as = case unsafePerformIO $ timeout microseconds $ return $ evalHead as of
--                   Just [] -> []
--                   Just (a:as) -> a : aux as
--                   Nothing -> [nothing { ok = Just False }]
--        evalHead [] = []
--        evalHead (a:as) = seq (ok a) (a:as)

data MTTF = MTTFGreaterThan Double Double | Inconclusive | MTTF Double Double
  deriving (Show)

mttfToCSV :: MTTF -> [String]
mttfToCSV (MTTFGreaterThan tests time) = ["MTTFGreaterThan", show tests, show time]
mttfToCSV Inconclusive = ["Inconclusive", "0", "0"]
mttfToCSV (MTTF tests time) = ["MTTF", show tests, show time]

-- TODO: Failures
statsToCSV :: (Double, Int, Double, Int) -> [String]
statsToCSV (d1, _, d2, i) = [show d1, show d2, show i]

-- | Generates a value that satisfies a predicate.
suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p =
  do
    mx <- gen `suchThatMaybe` p
    case mx of
      Just x -> return x
      Nothing -> sized (\n -> resize (n + 1) (gen `suchThat` p))

suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
gen `suchThatMaybe` p = sized (\n -> try n (2 * n))
  where
    try m n
      | m > n = return Nothing
      | otherwise = do
        x <- resize m gen
        if p x then return (Just x) else try (m + 1) n
