module Data.String.Batteries
  ( toLowerS
  , ordS, alphaOrdS
  ) where

import Data.Char.Batteries

import Data.Char

toLowerS :: String -> String
toLowerS = map toLower

ordS :: String -> Int
ordS = sum . map ord

alphaOrdS :: String -> Int
alphaOrdS = sum . map alphaOrd
