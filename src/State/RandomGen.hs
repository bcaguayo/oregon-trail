{-
---
fulltitle: "In class exercise: Random Generation"
date: November 1, 2023
---
-}

module RandomGen where

-- Some library operations that you can use for this exercise.
import qualified Control.Monad as Monad
-- Make sure you have filled in all of the 'undefined' values in the State module.
-- If you have not, modify the State import below to Control.Monad.State
-- but don't import both State and Control.Monad.State
import qualified State.State as S
import System.Random (StdGen, Random (randomR))
import qualified System.Random as Random (mkStdGen, randomIO, uniform, uniformR, getStdGen)

-- It also might be tempting to import Test.QuickCheck, but do not import anything
-- from quickcheck for this exercise.

{-
Random Generation
-----------------

Recall that QuickCheck needs to randomly generate values of any type. It turns
out that we can use the state monad to define something like the `Gen` monad
used in the QuickCheck libary.

First, a brief discussion of pseudo-random number generators. [Pseudo-random
number generators](http://en.wikipedia.org/wiki/Pseudorandom_number_generator)
aren't really random, they just look like it. They are more like functions
that are so complicated that they might as well be random. The nice property
about them is that they are repeatable, if you give them the same *seed* they
produce the same sequence of "random" numbers.

Haskell has a library for Pseudo-Random numbers called
[`System.Random`](http://hackage.haskell.org/packages/archive/random/latest/doc/html/System-Random.html).
It features the following elements:

~~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
type StdGen  -- A type for a "standard" random number generator.
             -- Keeps track of the current seed.

-- | Construct a generator from a given seed. Distinct arguments
-- are likely to produce distinct generators.
mkStdGen :: Int -> StdGen

-- The `uniform` function is overloaded, but we will only use two instances of
-- it today.
-}

-- | Returns an Int that is uniformly distributed in a range of at least 30 bits.
uniformInt :: StdGen -> (Int, StdGen)
uniformInt = Random.uniform

-- | Returns True / False with even chances
uniformBool :: StdGen -> (Bool, StdGen)
uniformBool = Random.uniform

{-
~~~~~~~~~~~~~~~~~~~~~~~~~~

Side note: the default constructor `mkStdGen` is a bit weak so we wrap it to
perturb the seed a little first:
-}

mkStdGen :: Int -> StdGen
mkStdGen = Random.mkStdGen . (* (3 :: Int) ^ (20 :: Int))

{-
For example, we can generate a random integer by constructing a random
number generator, calling `uniform` and then projecting the result.
-}

testRandom :: Int -> Int
testRandom i = fst (uniformInt (mkStdGen i))

{-
Our random integers depend on the seed that we provide. Make sure that you
get different numbers from these three calls.
-}

-- this is Minecraft 

-- >>> testRandom 1
-- -8728299723972136512

-- >>> testRandom 2
-- 7133861895013252414

-- >>> testRandom 3
-- 5757771102651567923

{-
But we can also produce several different random `Int`s by using the
output of one call to `Random.uniform` as the input to the next.
-}

(int1 :: Int, stdgen1) = uniformInt (mkStdGen 1)

(int2 :: Int, stdgen2) = uniformInt stdgen1

(int3 :: Int, _) = uniformInt stdgen2

-- >>> int1
-- -8728299723972136512

-- >>> int2
-- 1508247628499543321

-- >>> int3
-- 4708425006071971359

{-
If we'd like to constrain that integer to a specific range `(0, n)`
we can use the mod operation.
-}

nextBounded :: Int -> StdGen -> (Int, StdGen)
nextBounded bound s = let (x, s1) = uniformInt s in (x `mod` bound, s1)

{-
These tests should all produce random integers between 0 and 20.
-}

testBounded :: Int -> Int
testBounded = fst . nextBounded 20 . mkStdGen

-- >>> testBounded 1
-- 8

-- >>> testBounded 2
-- 14

-- >>> testBounded 3
-- 3

{-
QuickCheck is defined by a class of types that can construct random
values. Let's do it first the hard way... i.e. by explicitly passing around the
state of the random number generator.

-}

-- | Extract random values of any type
class Arb1 a where
  arb1 :: StdGen -> (a, StdGen)

instance Arb1 Int where
  arb1 :: StdGen -> (Int, StdGen)
  arb1 = uniformInt

instance Arb1 Bool where
  arb1 :: StdGen -> (Bool, StdGen)
  arb1 = uniformBool

{-
With this class, we can also generalize our "testing" function.
-}

testArb1 :: Arb1 a => Int -> a
testArb1 = fst . arb1 . mkStdGen

{-
What about for pairs? Note that Haskell needs the type annotations for
the two calls to `arb1` to resolve ambiguity.
-}

instance (Arb1 a, Arb1 b) => Arb1 (a, b) where
  arb1 :: StdGen -> ((a, b), StdGen)
  arb1 s =
    let (a :: a, s1) = arb1 s
        (b :: b, s2) = arb1 s1
     in ((a, b), s2)

{-
Try out this definition, noting the different integers in the two
components in the pair. If both calls to `arb1` above used `s`, then we'd get
the same number in both components.
-}

-- >>> testArb1 1 :: (Int, Int)
-- (-8728299723972136512,1508247628499543321)
{-
>
-}

-- >>> testArb1 2 :: (Int, Int)
-- (7133861895013252414,-3695387158857804490)

{-
How about for the `Maybe` type? Use the `arb1` instance for the `Bool` type
 above to generate a random boolean and then test it to decide whether you
should return `Nothing` or `Just a`, where the `a` also comes from `arb1`.
-}

instance (Arb1 a) => Arb1 (Maybe a) where
  arb1 :: StdGen -> (Maybe a, StdGen)
  arb1 s = -- if rBool then (Just rInt, s) else (Nothing, s) where
    let (a :: a, s1) = arb1 s
        (b :: Bool, s2) = arb1 s1 in 
    if b then (Just a, s2) else (Nothing, s2)
  -- arb1 s = if bol then (Just , s) else (Nothing, s) where
  --   intA = 
  --   boolA = fst (uniformBool s)

{-
And for lists? Give this one a try!  Although we don't have QCs combinators
available, you should be able to control the frequency of when cons and nil
is generated so that you get reasonable lists.

-}

instance Arb1 a => Arb1 [a] where
  arb1 s = helper i s where
    -- (a :: a, s1) = arb1 s
    (i :: Int, s1) = nextBounded 10 s
    -- f :: StdGen -> (a, StdGen)
    -- f = \ s -> arb1 s
      -- \ ((a :: a), s1) f s1)

-- use forall a for internal type signature
helper :: forall a. Arb1 a => Int -> StdGen -> ([a], StdGen)
helper 0 s = ([], s)
helper i s = (fst loop : fst taill, snd taill) where
  loop :: (a, StdGen)
  loop = arb1 s
  taill :: ([a], StdGen)
  taill = helper (i - 1) (snd loop)

-- instance Arb1 a => Arb1 [a] where
--   arb1 s = (lift i s, s) where


-- getNew :: Arb1 a => a -> (a, StdGen)
-- getNew s = let (a :: a, s1) = arb1 s in (a, s1)

-- >>> testArb1 1 :: [Int]

-- >>> testArb1 2 :: [Int]

-- >>> testArb1 3 :: [Int]

{-
Ouch, there's a lot of state passing going on here.

State Monad to the Rescue
-------------------------

Previously, we have developed a reusable library for the State monad.
Let's use it to *define* a generator monad for QuickCheck.

Our reusable library defines an abstract type for the state monad, and
the following operations for working with these sorts of computations.

~~~~~~~~~~~~~~~~~~~~~~~~{.haskell}
type State s a = ...

instance Monad (State s) where ...

get      :: State s s
put      :: s -> State s ()

runState :: State s a -> s -> (a,s)
~~~~~~~~~~~~~~~~~~~~~~~~~~

Now let's define a type for generators, using the state monad.
-}

type Gen a = S.State StdGen a

{-
With this type, we can create a type class similar to the one in the
QuickCheck library.
-}

class Arb a where
  arb :: Gen a

{-
For example, we can use the operations on the state monad to access and update the
random number generator stored in the `State StdGen a` type.
-}

instance Arb Int where
  arb :: Gen Int
  arb = do
    s <- S.get
    let (y :: Int, s') = Random.uniform s
    S.put s'
    return y

{-
What if we want a bounded generator? See if you can define one without using `Random.uniformR`.
-}

bounded :: Int -> Gen Int
bounded b = do
  x <- S.get
  let (y :: Int, s') = Random.uniform x
  S.put s'
  return (mod y b)

bounded' :: Int -> Gen Int
bounded' b = arb >>= \ y -> return (mod y b)

bounded'' :: Int -> Gen Int
bounded'' b = fmap (mod b) arb

{-
Now define a `sample` function, which generates and prints 10 random values.
-}

sample :: Show a => Gen a -> IO ()
sample gen = do
  seed <- (Random.randomIO :: IO Int) -- get a seed from the global random number generator
  -- hidden in the IO monad
  let (x, _) = S.runState gen (mkStdGen seed)
  print x

{-
For example, you should be able to sample using the `bounded` combinator.

    ghci> sample (bounded 10)
    5
    9
    0
    5
    4
    6
    0
    0
    7
    6

What about random generation for other types?  How does the state
monad help that definition? How does it compare to the version above?
-}

instance (Arb a, Arb b) => Arb (a, b) where
  arb = arb >>= \ a -> arb >>= \ b -> return (a, b)

arb' :: (Arb a, Arb b) => Gen (a, b)
arb' = Monad.liftM2 (,) arb arb

{-
Can we define some standard QuickCheck combinators to help us?
What about `elements`, useful for the `Bool` instance ?
-}

elements :: [a] -> Gen a
elements xs = do
  i <- bounded (length xs)
  return (xs !! 1)

instance Arb Bool where
  arb :: Gen Bool
  arb = elements [False, True]

{-
or `frequency`, which we can use for the `[a]` instance ?
-}

frequency :: [(Int, Gen a)] -> Gen a
frequency xs = undefined
  -- do
  -- -- gen <- Random.getStdGen
  -- let total = sum (map fst xs)
  -- let n = arb :: Gen Int
  -- -- let (r, newGen) = randomR (1 :: Int, 10 :: Int) gen
  -- -- n <- Random.randomIO-- (1, total)
  -- pick n xs
  -- where
  --   pick n ((k, gen) : rest)
  --     | n <= k = gen
  --     | otherwise = pick (n - k) rest
  --   pick _ [] = error "Empty frequency list"

instance (Arb a) => Arb [a] where
  arb :: Arb a => Gen [a]
  arb = frequency [(1, return []), (3, (:) <$> arb <*> arb)]
