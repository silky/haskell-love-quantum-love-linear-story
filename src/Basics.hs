{-# LANGUAGE LinearTypes #-}

module Basics where

-- | Not possible to re-use x here.
myId :: Int %1-> Int
myId x = x

-- | This is now the _only_ implementation possible.
myId' :: a %1 -> a
myId' a = a

-- | Cannot be implemented.
clone :: a -> (a, a)
clone = undefined
