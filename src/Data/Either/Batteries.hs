{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric, DeriveTraversable #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Either.Batteries
  ( -- ** Contructors
    ifThenEither
    -- ** Conversions
    , mirrorE
      -- *** from Maybe
  , blame, only
  , blameMap, onlyMap
      -- *** to Maybe
  , maybeRight, maybeLeft
    -- ** Maps
  , mapRight, mapLeft
    -- ** Unwrapping
      -- *** Safe
  , rightOr, leftOr
  , handleLeft, handleRight
      -- *** Unsafe
    , fromRight, fromLeft, expect, reject
    , fromRight', fromLeft', expect', reject'
    -- ** Conjunctions and disjunctions
  , andE, andThen, andWith
  , orE, orElse, orWith
    -- * `Monoid` wrappers
    -- $MonoidWrappers

    -- ** Conjunctive Wrappers
    -- $ConjunctiveWrappers
  , AndRight(..), AndLeft(..), AndEither(..)
    -- ** Disjunctive Wrappers
    -- $DisjunctiveWrappers
  , OrRight(..), OrLeft(..), OrEither(..)
  ) where

import Control.Monad.Fix
import Data.Bifunctor
import Data.Data
import Data.Functor.Classes
import GHC.Generics

infixr 3 `andE`
infixr 3 `andWith`
infixr 2 `orE`
infixr 2 `orWith`

infixl 1 `andThen`
infixl 1 `orElse`

ifThenEither :: Bool -> e -> a -> Either e a
ifThenEither cond l r = if cond then Left l else Right r

-- | Maps `Just` to `Right` and Nothing` to the provided error value.
blame :: e -> Maybe a -> Either e a
blame _ (Just a) = Right a
blame e Nothing  = Left e

-- | Maps `Just` to `Left` and Nothing` to the provided success value.
only :: a -> Maybe e -> Either e a
only a Nothing  = Right a
only _ (Just e) = Left e

-- | Maps Nothing` to the provided error value,
-- and `Just` to `Right`, applying f to the contained value,
blameMap :: e -> (a -> b) -> Maybe a -> Either e b
blameMap _ f (Just a) = Right (f a)
blameMap e _ Nothing  = Left e

-- | Maps `Nothing` to the provided success value
-- and `Just` to `Left`, applying f to the contained value.
onlyMap :: a -> (e -> f) -> Maybe e -> Either f a
onlyMap a _ Nothing  = Right a
onlyMap _ f (Just e) = Left (f e)

-- | Discards the error value if any.
maybeRight :: Either e a -> Maybe a
maybeRight (Right a) = Just a
maybeRight (Left  _) = Nothing

-- | Discards the success value if any.
maybeLeft :: Either e a -> Maybe e
maybeLeft (Left  e) = Just e
maybeLeft (Right _) = Nothing

-- | Converts a `Right` into a `Left`
-- and a `Left` into a `Right`.
mirrorE :: Either b a -> Either a b
mirrorE = either Right Left

-- | Maps over an `Either`, applying a function to a contained `Right` value,
-- leaving a `Left` value untouched.
--
-- Same as `fmap`.
mapRight :: (a -> b) -> Either e a -> Either e b
mapRight f (Right a) = Right (f a)
mapRight _ (Left  e) = Left e

-- | Maps over an `Either`, applying a function to a contained `Left` value,
-- leaving a `Right` value untouched.
mapLeft :: (e -> f) -> Either e a -> Either f a
mapLeft _ (Right a) = Right a
mapLeft f (Left  e) = Left (f e)

-- | Unwraps an `Either`, yielding the content of a `Right`
-- or defaults to the value provided.
rightOr :: a -> Either e a -> a
rightOr val = either (const val) id

-- | Unwraps an `Either`, yielding the content of a `Left`
-- or defaults to the value provided.
leftOr :: e -> Either e a -> e
leftOr val = either id (const val)

-- | Unwraps an `Either`, yielding the content of a `Right`
-- or calls a function on the `Left` value.
handleLeft :: (e -> a) -> Either e a -> a
handleLeft f = either f id

{-# ANN handleRight "HLint: ignore Eta reduce" #-}
-- | Unwraps an `Either`, yielding the content of a `Left`
-- or calls a function on the `Right` value.
handleRight :: (a -> e) -> Either e a -> e
handleRight f = either id f

-- | Unwraps an `Either`, yielding the content of a `Right`.
--
-- Produces an error if the value is a `Left`.
fromRight' :: Either e a -> a
fromRight' = expect' "fromRight: Left"

-- | Unwraps an `Either`, yielding the content of a `Left`.
--
-- Produces an error if the value is a `Right`.
fromLeft' :: Either e a -> e
fromLeft' = reject' "fromLeft: Right"

-- | Unwraps an `Either`, yielding the content of a `Right`.
--
-- Produces an error if the value is a `Left`,
-- with a message provided by the `String`.
expect' :: String -> Either e a -> a
expect' _      (Right a) = a
expect' errMsg (Left  _) = error errMsg

-- | Unwraps an `Either`, yielding the content of a `Left`.
--
-- Produces an error if the value is a `Right`,
-- with a message provided by the `String`.
reject' :: String -> Either e a -> e
reject' _      (Left  e) = e
reject' errMsg (Right _) = error errMsg

-- | Produces an error with a message and a printable value.
errorMsgVal :: (Show value) => String -> value -> a
errorMsgVal msg value = error $ msg ++ " " ++ show value

-- | Unwraps an `Either`, yielding the content of a `Right`.
--
-- Produces an error if the value is a `Left`,
-- with a message provided by the `Left`'s value.
fromRight :: (Show e) => Either e a -> a
fromRight = expect "fromRight: Left"

-- | Unwraps an `Either`, yielding the content of a `Left`.
--
-- Produces an error if the value is a `Right`,
-- with a message provided by the `Right`'s value.
fromLeft :: (Show a) => Either e a -> e
fromLeft = reject "fromLeft: Right"

-- | Unwraps an `Either`, yielding the content of a `Right`.
--
-- Produces an error if the value is a `Left`, with a message
-- provided by the `String` and the content of the Left.
expect :: (Show e) => String -> Either e a -> a
expect _      (Right a) = a
expect errMsg (Left  e) = errorMsgVal errMsg e

-- | Unwraps an `Either`, yielding the content of a `Left`.
--
-- Produces an error if the value is a `Right`, with a message
-- provided by the `String` and the content of the Right.
reject :: (Show a) => String -> Either e a -> e
reject _      (Left  e) = e
reject errMsg (Right a) = errorMsgVal errMsg a

-- | Returns the last `Either` if all previous were successful,
-- or at the first failure encountered.
andE :: Either e a -> Either e b -> Either e b
Right _ `andE` r = r
Left  e `andE` _ = Left e

-- | Applies a function to the content of an `Either` on success,
-- or returns the first `Left` found.
--
-- Same as (`>>=`).
andThen :: Either e a -> (a -> Either e b) -> Either e b
andThen = (>>=)

-- | Merges `Right` values using a given function
-- or returns the first `Left` found.
--
-- Same behaviour as `AndRight`.
andWith :: Either e a -> (a -> b -> c) -> Either e b -> Either e c
andWith (Right b) f (Right a) = Right (f b a)
andWith (Right _) _ (Left  e) = Left e
andWith (Left  e) _ _         = Left e

-- | Returns the first successful `Either`,
-- or the last `Left` if only errors were found.
orE :: Either e a -> Either f a -> Either f a
Right a `orE` _ = Right a
Left  _ `orE` r = r

-- | Returns the first successful `Either`
-- or applies a function to the content of a `Left` on failure.
orElse :: Either e a -> (e -> Either f a) -> Either f a
Right a `orElse` _ = Right a
Left  e `orElse` f = f e

-- | Merges `Right` values using a given function
-- or returns the last `Left` if no `Right` was found.
--
-- Same behaviour as `OrRight`.
orWith :: Either e a -> (a -> a -> a) -> Either f a -> Either f a
orWith (Right a) f (Right b) = Right (f a b)
orWith (Right a) _ (Left  _) = Right a
orWith (Left  _) _ r         = r

-- $MonoidWrappers
-- These wrappers are useful if either your success type or your failure type
-- (or both) are instances of the `Monoid` class. You can then `mappend` your
-- results or your errors.
--
-- These wrappers come in two flavours: conjunctive and disjunctive. Each flavour
-- has three declensions to accommodate for any combination of `Monoid` instances.

-- $ConjunctiveWrappers
-- The conjunctive family discards `Right`s in favour of `Left`s. You should use:
--
--   * `AndRight` to `mappend` on `Right` values.
--   * `AndLeft` to `mappend` on `Left` values.
--   * `AndEither` to `mappend` on both `Right` and `Left` values.
--
-- When `Monoid` is not available:
--
--   * `AndRight` will return the first `Left` encountered.
--   * `AndLeft` will return the last `Right` encountered (if no `Left` was found).
--
-- NB: When using `AndLeft`, `mempty` is an `Left` value.

-- | `Either` monoid on `Right`.
newtype AndRight e a = AndRight { getAndRight :: Either e a }
  deriving ( Show, Read, Eq, Ord, Generic, Show1, Read1, Eq1, Ord1, Generic1, Show2, Read2, Eq2, Ord2
           , Functor, Bifunctor, Applicative, Monad, MonadFix, Foldable, Traversable, Data)

instance (Semigroup a) => Semigroup (AndRight e a) where
  AndRight (Right a) <> AndRight (Right b) = AndRight (Right (a <> b))
  AndRight (Right _) <> AndRight (Left e)  = AndRight (Left e)
  AndRight (Left e)  <> _                  = AndRight (Left e)

instance (Monoid a) => Monoid (AndRight e a) where
  mempty = AndRight (Right mempty)

-- | `Either` monoid on `Left`.
newtype AndLeft e a = AndLeft { getAndLeft :: Either e a }
  deriving ( Show, Read, Eq, Ord, Generic, Show1, Read1, Eq1, Ord1, Generic1, Show2, Read2, Eq2, Ord2
           , Functor, Bifunctor, Applicative, Monad, MonadFix, Foldable, Traversable, Data)

instance (Semigroup e) => Semigroup (AndLeft e a) where
  AndLeft (Right _) <> andLeft           = andLeft
  AndLeft (Left e)  <> AndLeft (Right _) = AndLeft (Left e)
  AndLeft (Left e)  <> AndLeft (Left f)  = AndLeft (Left (e <> f))

instance (Monoid e) => Monoid (AndLeft e a) where
  mempty = AndLeft (Left mempty)

-- | `Either` monoid on `Right` and `Left`.
newtype AndEither e a = AndEither { getAndEither :: Either e a }
  deriving ( Show, Read, Eq, Ord, Generic, Show1, Read1, Eq1, Ord1, Generic1, Show2, Read2, Eq2, Ord2
           , Functor, Bifunctor, Applicative, Monad, MonadFix, Foldable, Traversable, Data)

instance (Semigroup e, Semigroup a) => Semigroup (AndEither e a) where
  AndEither (Right a) <> AndEither (Right b) = AndEither (Right (a <> b))
  AndEither (Right _) <> AndEither (Left e)  = AndEither (Left e)
  AndEither (Left e)  <> AndEither (Right _) = AndEither (Left e)
  AndEither (Left e)  <> AndEither (Left f)  = AndEither (Left (e <> f))

instance (Monoid e, Monoid a) => Monoid (AndEither e a) where
  mempty = AndEither (Right mempty)

-- $DisjunctiveWrappers
-- The disjunctive family discards `Left`s in favour of `Right`s. You should use:
--
--   * `OrRight` to `mappend` on `Right` values.
--   * `OrLeft` to `mappend` on `Left` values.
--   * `OrEither` to `mappend` on both `Right` and `Left` values.
--
-- When `Monoid` is not available:
--
--   * `OrRight` will return the last `Left` encountered (if no `Right` was found).
--   * `OrLeft` will return the first `Right` encountered.
--
-- NB: When using `OrRight`, `mempty` is an `Right` value.

-- | `Either` monoid on `Right`.
newtype OrRight e a = OrRight { getOrRight :: Either e a }
  deriving ( Show, Read, Eq, Ord, Generic, Show1, Read1, Eq1, Ord1, Generic1, Show2, Read2, Eq2, Ord2
           , Functor, Bifunctor, Applicative, Monad, MonadFix, Foldable, Traversable, Data)

instance (Semigroup a) => Semigroup (OrRight e a) where
  OrRight (Right a) <> OrRight (Right b) = OrRight (Right (a <> b))
  OrRight (Right a) <> OrRight (Left _)  = OrRight (Right a)
  OrRight (Left _)  <> orRight           = orRight

instance (Monoid a) => Monoid (OrRight e a) where
  mempty = OrRight (Right mempty)

-- | `Either` monoid on `Left`.
newtype OrLeft e a = OrLeft { getOrLeft :: Either e a }
  deriving ( Show, Read, Eq, Ord, Generic, Show1, Read1, Eq1, Ord1, Generic1, Show2, Read2, Eq2, Ord2
           , Functor, Bifunctor, Applicative, Monad, MonadFix, Foldable, Traversable, Data)

instance (Semigroup e) => Semigroup (OrLeft e a) where
  OrLeft (Right a) <> _                = OrLeft (Right a)
  OrLeft (Left _)  <> OrLeft (Right a) = OrLeft (Right a)
  OrLeft (Left e)  <> OrLeft (Left f)  = OrLeft (Left (e <> f))

instance (Monoid e) => Monoid (OrLeft e a) where
  mempty = OrLeft (Left mempty)

-- | `Either` monoid on `Right` and `Left`.
newtype OrEither e a = OrEither { getOrEither :: Either e a }
  deriving ( Show, Read, Eq, Ord, Generic, Show1, Read1, Eq1, Ord1, Generic1, Show2, Read2, Eq2, Ord2
           , Functor, Bifunctor, Applicative, Monad, MonadFix, Foldable, Traversable, Data)

instance (Semigroup e, Semigroup a) => Semigroup (OrEither e a) where
  OrEither (Right a) <> OrEither (Right b) = OrEither (Right (a <> b))
  OrEither (Right a) <> OrEither (Left _)  = OrEither (Right a)
  OrEither (Left _)  <> OrEither (Right a) = OrEither (Right a)
  OrEither (Left e)  <> OrEither (Left f)  = OrEither (Left (e <> f))

instance (Monoid e, Monoid a) => Monoid (OrEither e a) where
  mempty = OrEither (Left mempty)
