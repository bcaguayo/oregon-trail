module Trace where

import Control.Monad (ap, liftM, (>=>))
import qualified Control.Monad.State as S
import qualified System.IO as IO
import Test.HUnit

-- InputB for Input Blocking
class Monad m => InputB m where
  inputb :: m String

class Monad m => Output m where
  output :: String -> m ()

instance InputB IO where
  inputb = getLine

instance Output IO where
  output = putStrLn

-- | Wait for some input to appear, and when it does, repeat it.
echo :: (InputB m, Output m) => m ()
echo = inputb >>= output >> output "\n"

data TraceBIO a
  = Done a
  | Output String (TraceBIO a)
  | Input (String -> TraceBIO a)  

{-
For example, the trace of the echo program above looks like this:
-}

echoTrace :: TraceBIO ()
echoTrace = Input ( \str -> Output str (Output "\n" (Done ())) )

{-
A test case can the "run" the trace, with a specific list of inputs to
mock what happens during an execution.
-}

runTraceBIO :: TraceBIO () -> [String] -> [String]
runTraceBIO (Done _) inputs = []
runTraceBIO (Output s dio) inputs = s : runTraceBIO dio inputs
runTraceBIO (Input f) (x : xs) = runTraceBIO (f x) xs
runTraceBIO (Input f) [] = runTraceBIO (f "") []

{-
However, for a given program like `echo`, we don't want to have
to construct its trace by hand. We'd like to test the original program.
Fortunately, `echo` is generic over the Monad that we use for execution.
So by making the `TraceBIO` type an instance of the `Monad` type class,
we can extract the `echoTrace` definition directly from the `echo` program
itself.
-}

instance Monad TraceBIO where
  return :: a -> TraceBIO a
  return = pure
  (>>=) :: TraceBIO a -> (a -> TraceBIO b) -> TraceBIO b
  (>>=) (Done x) k = k x
  -- use bind to put two traces together
  (>>=) (Output str f) k = Output str (f >>= k)
  -- input if a function, so we need to apply it to the input
  (>>=) (Input f) k = Input (\ms -> f ms >>= k)

{-
(As usual, the Applicative and Functor instances can be derived from the
Monad instance.)
-}

instance Applicative TraceBIO where
  pure :: a -> TraceBIO a
  pure = Done
  (<*>) :: TraceBIO (a -> b) -> TraceBIO a -> TraceBIO b
  (<*>) = ap

instance Functor TraceBIO where
  fmap :: (a -> b) -> TraceBIO a -> TraceBIO b
  fmap = liftM

{-
Furthemore, to test the `echo` example, we need instances of the `Input` and `Ouput` classes. These instances use
the data constructors to record the interactions.
-}

instance Output TraceBIO where
  output :: String -> TraceBIO ()
  output s = Output s (return ())

instance InputB TraceBIO where
  inputb :: TraceBIO String
  inputb = Input return