{-# language LinearTypes #-}
{-# language QualifiedDo #-}
{-# OPTIONS_GHC -fno-defer-type-errors #-}

module Ex2 where

import qualified Prelude
import           System.IO.Resource        (RIO)
import qualified Control.Functor.Linear    as L

import           MonadicApi

-- Example 2. We can't get the parameters to `cnot` to be wrong.
ex2 :: RIO (Qubit, Qubit)
ex2 = L.do
  a <- allocate Zero
  b <- allocate Zero

  a <- h a
  -- Bad:
  -- (a, b) <- cnot a a
  -- Good:
  -- (a, b) <- cnot a b

  L.return (a, b)










