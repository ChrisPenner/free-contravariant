{-# LANGUAGE DeriveFunctor #-}

module Machines where

import FreeContravariant

countLetter :: Int -> Char -> FC Maybe Char
countLetter 0 _ = FC (const Nothing)
countLetter n c =
  FC $ \a ->
    pure $
    if c == a
      then countLetter (n - 1) c
      else countLetter n c

data Result a
  = Success a
  | Fail a
  deriving (Show, Eq, Functor)

instance Applicative Result where
  pure = Success
  Success f <*> Success a = Success (f a)
  Fail f <*> Success a = Fail (f a)
  Success f <*> Fail a = Fail (f a)
  Fail f <*> Fail a = Fail (f a)

instance Monad Result where
  Fail a >>= f = f a
  Success a >>= f = f a

matchedParens :: FC Result Char
matchedParens = FC $ helper 0

helper :: Int -> Char -> Result (FC Result Char)
helper n '(' = Fail (FC $ helper (n + 1))
helper 1 ')' = Success (FC $ helper 0)
helper n ')' = Fail (FC $ helper (n - 1))
helper 0 _ = Success (FC $ helper 0)
helper n _
  | n > 0 = Fail (FC $ helper n)
  -- | n < 0 = Fail failFail

-- helper n _
failFail :: FC Result a
failFail = FC $ \_ -> Fail failFail
