{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a =
  Compose (f (g a))

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) =>
    Functor (Compose f g) where
  f <$> (Compose a) = Compose ((\ a0 -> f <$> a0) <$> a)
    -- error "todo: Course.Compose (<$>)#instance (Compose f g)"

instance (Applicative f, Applicative g) =>
  Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . pure . pure
    -- error "todo: Course.Compose pure#instance (Compose f g)"
-- Implement the (<*>) function for an Applicative instance for Compose
  (Compose f) <*> (Compose a) =
    Compose (((\ g -> (g <*>)) <$> f) <*> a)
    -- error "todo: Course.Compose (<*>)#instance (Compose f g)"

instance (Monad f, Monad g) =>
  Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  (=<<) =
    error "not possible"
