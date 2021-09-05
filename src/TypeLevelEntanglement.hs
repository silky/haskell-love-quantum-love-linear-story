{-# language LinearTypes    #-}
{-# language QualifiedDo    #-}
{-# language TypeOperators  #-}
{-# language DataKinds      #-}
{-# language GADTs          #-}
{-# language KindSignatures #-}
{-# OPTIONS_GHC -fno-defer-type-errors #-}

-- Some thoughts on representing entanglement at the type level.

module TypeLevelEntanglement where

import qualified Prelude
import           Prelude.Linear
import           Data.Unrestricted.Linear  (Ur (..))
import           System.IO.Resource        (RIO)
import qualified Control.Functor.Linear    as L

import           MonadicApi

-- Represents the tensor product of 'a' and 'b', a :*: b := a ⊗ b.
infixr 7 :*:
data a :*: b = a :*: b

-- Represents a list of qubits that are entangled. For example, the Bell
-- state.
data Entangled :: [*] -> * where
  Empty :: Entangled '[]
  Cons  :: a %1 -> Entangled t %1 -> Entangled (a ': t)

-- This function works on tensor-products only. I.e. qubits that can be
-- seperated.
op1 :: Qubit :*: Qubit %1 -> RIO Qubit
op1 (a :*: b) = L.do
  Ur _ <- measure_ a
  b <- h b
  L.return b

-- Here we produce the Bell state in a way that indicates the entanglement;
-- i.e. it's not returned as a tensor-product.
op2 :: RIO (Entangled '[Qubit, Qubit])
op2 = L.do
  a <- allocate Zero
  b <- allocate Zero

  a <- h a           -- Line 1
  (a, b) <- cnot a b -- Line 2

  L.return (a `Cons` (b `Cons` Empty))

-- Issue 1) If either "Line 1" or "Line 2" is commented out (or both), no
-- entanglement is created between qubits 'a' and 'b'. The types don't reflect
-- this! That's a shame; and a real issue: Neither operation alone creates
-- entanglement, so how to indicate that the combined operation does?
--
-- Issue 2) Even if we solved that, entanglement isn't binary! We need a type
-- to encode the _amount_ of entanglement, 'e'. Perhaps something like:

data Entangled' e :: [*] -> * where
  Empty':: Num e => Entangled' e '[]
  Cons' :: Num e => a %1 -> Entangled' e t %1 -> Entangled' e (a ': t)

-- Then maybe we could define a function that always reduces entanglement.

-- | We want to say this function is something like 'e -> e - Δe`; i.e.
-- it reduces entanglement by a little bit. How to do that?
reduceEntanglement :: Entangled' e' qubits %1 -> Entangled' e qubits
reduceEntanglement = undefined

-- There are still a few problems:
--
-- * When 'e' is zero, we should learn that the qubits are seperable. I.e.
--   we should have the following type equality
--
--    Entangled' 0 '[Qubit, Qubit, ...] ~ Qubit :*: Qubit :*: ...
--
-- * 'e' is some measure of 'entanglement'; but which one? There are options:
--    <https://en.wikipedia.org/wiki/Quantum_entanglement#Entanglement_measures>
--
-- * A single 'e' is almost definitely not enough. In a list of 10 qubits, it
--   may be that two are entangled and the rest are not. How  to represent
--   this?

-- Related ideas:
--  * Weak measurement: <https://en.wikipedia.org/wiki/Weak_measurement>

