{-# language LinearTypes       #-}
{-# language NoImplicitPrelude #-}
{-# language QualifiedDo       #-}

module Forge where

import           Prelude.Linear
import           System.IO.Resource        (RIO)
import qualified Control.Functor.Linear    as L
import           MonadicApi

type Money = (Qubit, Qubit, Qubit)

-- forge :: Money %1 -> (Money, Money)
-- forge m = (m, m)

spend :: Qubit %1 -> RIO ()
spend = shutdown
