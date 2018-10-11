{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE UndecidableInstances #-}

module ProC where

import Control.Applicative
import Control.Arrow as A
import Control.Comonad
import Control.Monad
import Data.Bifoldable as B
import Data.Bifunctor as B
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Profunctor
import Data.Void

newtype FC f b a = FC
  { runFC :: (b -> f a (FC f b a))
  }

instance Bifunctor f => Profunctor (FC f) where
  lmap :: forall a b c. (a -> b) -> FC f b c -> FC f a c
  lmap f (FC fc) = FC (B.second (lmap f) <$> lmap f fc)
  rmap :: forall a b c. (b -> c) -> FC f a b -> FC f a c
  rmap f (FC fc) = FC (B.second (rmap f) . B.first f <$> fc)
  -- contramap :: forall a b. (a -> b) -> FC f b -> FC f a
  -- contramap f (FC fc) = FC (fmap (contramap f) <$> (fc . f))
-- instance Alternative f => Divisible (FC f) where
--   divide :: forall a b c. (a -> (b, c)) -> FC f b -> FC f c -> FC f a
--   divide split (FC cvb) (FC cvc) =
--     FC $ \a ->
--       let (b, c) = split a
--           fb :: f (FC f a)
--           fb = contramap (fst . split) <$> cvb b
--           fc :: f (FC f a)
--           fc = contramap (snd . split) <$> cvc c
--        in fb <|> fc
--   conquer :: FC f a
--   conquer = FC (const empty)
-- divvy :: Alternative f => (a -> (b, c)) -> FC f b -> FC f c -> FC f a
-- divvy split (FC cvb) (FC cvc) =
--   FC $ \a ->
--     let (b, c) = split a
--         blahFst :: f (FC f a)
--         blahFst = (fmap (\nextB -> divide split nextB nextC)) cvb b <|> 
--      in undefined
-- instance Alternative f => Decidable (FC f) where
--   lose :: (a -> Void) -> FC f a
--   lose f = FC (absurd <$> f)
--   choose :: (a -> Either b c) -> FC f b -> FC f c -> FC f a
--   choose choice (FC fb) (FC fc) =
--     FC $ \a ->
--       case choice a of
--         Left b -> flip (choose choice) (FC fc) <$> (fb b)
--         Right c -> choose choice (FC fb) <$> (fc c)
-- step :: a -> FC f a -> f (FC f a)
-- step = flip runFC
-- joinStep :: Monad m => a -> FC m a -> FC m a
-- joinStep a (FC m) =
--   FC $ \a' -> do
--     FC m' <- m a
--     m' a'
-- aStep :: Alternative m => a -> FC m a -> FC m a
-- aStep a (FC m) =
--   FC $ \a' ->
--     let x = m a
--      in x
-- extractStep :: Comonad w => a -> FC w a -> FC w a
-- extractStep a (FC wa) = extract (wa a)
-- failTrue :: FC Maybe Bool
-- failTrue =
--   FC $ \case
--     True -> Nothing
--     False -> Just failTrue
-- failFalse :: FC Maybe Bool
-- failFalse = contramap not failTrue
-- hoist :: Functor g => (forall a. f a -> g a) -> FC f a -> FC g a
-- hoist f (FC cfa) = FC $ fmap (hoist f) . f . cfa
-- count :: FC (Compose [] ((,) Int)) Int
-- count =
--   FC $ \n ->
--     Compose [(n, contramap (+ n) count), (n + 1, contramap (+ (n + 1)) count)]
