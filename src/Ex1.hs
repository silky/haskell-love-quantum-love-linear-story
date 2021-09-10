{-# language LinearTypes #-}
{-# language QualifiedDo #-}
{-# OPTIONS_GHC -fno-defer-type-errors #-}

module Ex1 where

import qualified Prelude
import           System.IO.Resource        (RIO)
import qualified Control.Functor.Linear    as L

import           MonadicApi

-- Example 1. We can't allocate a qubit and forget about it.
ex1 :: RIO (Qubit)
ex1 = L.do
  a <- allocate Zero
  -- Good:
  --

  -- Bad:
  -- b <- allocate Zero

  -- Good again:
  -- b <- allocate Zero
  -- shutdown b

  L.return a
