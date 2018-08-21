module Data.Ix.Batteries
    ( fullRange
    ) where

import Data.Ix

fullRange :: (Bounded a, Ix a) => [a]
fullRange = range (minBound, maxBound)
