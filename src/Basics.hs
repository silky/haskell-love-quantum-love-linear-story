{-# language LinearTypes #-}

module Basics where

import           Data.Unrestricted.Linear  (Ur (..))
import qualified Prelude.Linear as L

-- | Not possible to re-use x here.
myId :: Int %1-> Int
myId x = x

-- | This is now the _only_ implementation possible.
myId' :: a %1 -> a
myId' a = a

-- | Note: This cannot be implemented.
clone :: a -> (a, a)
clone = undefined

-- | Pair an 'a' with a Boolean.
pair :: a %1 -> (a, Ur Bool)
pair a = (a, Ur True)

-- | We can take that pair and produce an infinite
-- number of copies of the Boolean; but not the 'a'.
f :: a %1 -> (a, [Bool])
f a = (\(a, Ur b) -> (a, repeat b)) (pair a)
