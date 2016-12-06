module Data.Function.Batteries
  ( const2
  , applyN
  ) where

const2 :: a -> b -> c -> a
const2 a _ _ = a

applyN :: Int -> (a -> a) -> a -> a
applyN n f = head . drop n . iterate f
