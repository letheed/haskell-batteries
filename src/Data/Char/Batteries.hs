module Data.Char.Batteries
  ( alphaOrd
  ) where

import Data.Char

alphaOrd :: Char -> Int
alphaOrd = subtract (ord 'A' - 1) . ord
