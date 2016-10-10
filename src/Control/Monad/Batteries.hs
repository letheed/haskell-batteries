module Control.Monad.Batteries
  ( (<<)
  ) where

(<<) :: (Monad m) => m b -> m a -> m b
(<<) = flip (>>)
