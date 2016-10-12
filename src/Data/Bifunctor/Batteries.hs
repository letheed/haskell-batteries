module Data.Bifunctor.Batteries
  ( both
  ) where

import Data.Bifunctor

both :: (Bifunctor p) => (a -> b) -> p a a -> p b b
both f = bimap f f
