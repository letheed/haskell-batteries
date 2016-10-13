module Data.Text.Batteries
  ( alphaOrdT
  ) where

import Data.Char.Batteries

import qualified Data.Text as T

alphaOrdT :: T.Text -> Int
alphaOrdT = sum . map alphaOrd . T.unpack
