{-# language LinearTypes #-}
{-# language QualifiedDo #-}
{-# OPTIONS_GHC -fno-defer-type-errors #-}

module LiveHackingExamples where

import qualified Prelude
import           Prelude.Linear
import           Data.Unrestricted.Linear  (Ur (..))
import           System.IO.Resource        (RIO)
import qualified Control.Functor.Linear    as L

import           MonadicApi

-- Example 1. We can't apply an operation and not use the resulting qubit.
ex1 :: RIO (Qubit)
ex1 = L.do
  a <- allocate Zero
  -- Bad:
  -- h a
  -- Good:
  -- a <- h a
  L.return a


-- Example 2. We can't allocate a qubit and forget about it.
ex2 :: RIO (Qubit)
ex2 = L.do
  a <- allocate Zero
  -- Good:
  --
  -- Bad:
  -- b <- allocate Zero
  L.return a


-- Example 3. We can't get the parameters to `cnot` to be wrong.
ex3 :: RIO (Qubit, Qubit)
ex3 = L.do
  a <- allocate Zero
  b <- allocate Zero

  a <- h a
  -- Bad:
  -- (a, b) <- cnot a a
  -- Good:
  (a, b) <- cnot a b

  L.return (a, b)
