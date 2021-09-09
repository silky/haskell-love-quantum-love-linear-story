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

-- Example 1. We can't allocate a qubit and forget about it.
ex1 :: RIO (Qubit)
ex1 = L.do
  a <- allocate Zero
  -- Good:
  --
  -- Bad:
  -- b <- allocate Zero
  L.return a
