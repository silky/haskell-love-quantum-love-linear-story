{-# language LinearTypes #-}
{-# language QualifiedDo #-}
{-# OPTIONS_GHC -fno-defer-type-errors #-}

module MonadicApi where

import           Prelude (undefined)
import qualified Prelude
import           Prelude.Linear

import           Data.Unrestricted.Linear  (Ur (..))
import           System.IO.Resource        (RIO)
import qualified Control.Functor.Linear    as L

data Qubit
data QubitInitialisation
  = Zero
  | One
  | Plus
  | Minus

-- | Measure a qubit, returning the qubit itself, and a boolean indicating
-- whether or not it was in the |0> state.
measure :: Qubit %1 -> RIO (Qubit, Ur Bool)
measure = undefined

-- | Same as 'measure', but throw away the qubit after measurement.
measure_ :: Qubit %1 -> RIO (Ur Bool)
measure_ q = L.do
  (q, r) <- measure q
  shutdown q
  L.return r

-- | Spin up a qubit in the given state.
allocate :: QubitInitialisation -> RIO Qubit
allocate = undefined

-- | Shutdown a specific qubit.
shutdown :: Qubit %1 -> RIO ()
shutdown = undefined

-- | Hadamard gate.
h :: Qubit %1 -> RIO Qubit
h = undefined

-- | 'X' gate or the 'Not' gate.
x :: Qubit %1 -> RIO Qubit
x = undefined

-- | 'Z' gate or 'Phase' gate.
z :: Qubit %1 -> RIO Qubit
z = undefined

-- | Identity gate.
i :: Qubit %1 -> RIO Qubit
i = undefined

-- | Generalised 2-qubit control gate.
c :: (Qubit %1 -> RIO Qubit) -- ^ Operation to apply conditionally.
  -> Qubit %1 -- ^ Source
  -> Qubit %1 -- ^ Target
  -> RIO (Qubit, Qubit)
c = undefined

-- | Controlled-Not gate.
cnot :: Qubit %1 -> Qubit %1 -> RIO (Qubit, Qubit)
cnot = c x

-- | Build the Bell state: (1/sqrt2) ( |00> + |11> )
bellState :: RIO (Qubit, Qubit)
bellState = L.do
  a <- allocate Zero
  b <- allocate Zero

  a <- h a
  (a, b) <- cnot a b

  L.return (a, b)

-- Examples from the Quipper paper: <https://arxiv.org/pdf/1304.5485.pdf>

-- | Comparision example from the 'Quipper' paper; now is a compilation error.
myLinearCirc :: Qubit %1 
             -> Qubit %1
             -> RIO (Qubit, Qubit)
myLinearCirc a b =
  L.do
    a <- h a
    b <- h b
    (a, b) <- cnot a b
    L.return (a, b)

-- Teleportation protocol from Quipper, implemented linearly. See page 5 of the
-- paper.

-- | Alice wants to teleport qubit 'q' to Bob. This function yields classical
-- measurement results that alice needs to communicate to Bob, in order for
-- Bob to obtain qubit `q` on his half  of the entangled state.
alice :: Qubit %1
      -> Qubit %1
      -> RIO (Ur Bool, Ur Bool)
alice q a = L.do
  (a, q) <- cnot a q
  q <- h q

  m1 <- measure_ q
  m2 <- measure_ a

  L.return (m1, m2)

-- |
bob :: Qubit %1
    -> (Ur Bool, Ur Bool) %1
    -> RIO Qubit
bob b (Ur m1, Ur m2) = L.do
  b <- if m1 then x b else L.pure $ id b
  b <- if m2 then z b else L.pure $ id b

  L.return b

-- | Teleport qubit 'q' from Alice to Bob. The return value of this function
-- is bob's qubit.
teleport :: Qubit %1 -> RIO Qubit
teleport q = L.do
  (a, b)   <- bellState
  (m1, m2) <- alice q a
  b <- bob b (m1, m2)
  L.return b
