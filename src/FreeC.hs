{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module FreeC where

import Control.Applicative
import Control.Arrow
import Control.Comonad
import Control.Monad
import Data.Functor.Compose
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void

newtype FC f a = FC
  { runFC :: (f (a -> FC f a))
  }

instance Functor f => Contravariant (FC f) where
  contramap :: forall a b. (a -> b) -> FC f b -> FC f a
  contramap f (FC fc) = FC (mapInner <$> fc)
    where
      mapInner :: (b -> FC f b) -> (a -> FC f a)
      mapInner fa = fmap (contramap f) fa . f

instance Alternative f => Divisible (FC f) where
  divide :: forall a b c. (a -> (b, c)) -> FC f b -> FC f c -> FC f a
  divide split (FC cvb) (FC cvc) = FC (thingB <$> cvb <|> thingC <$> cvc)
    where
      thingB :: (b -> FC f b) -> (a -> FC f a)
      thingB fb =
        let toB :: a -> b
            toB = fst <$> split
            t :: FC f b -> FC f a
            t = contramap toB
         in (\a -> t . fb . toB $ a)
      thingC :: (c -> FC f c) -> (a -> FC f a)
      thingC fc =
        let toC :: a -> c
            toC = snd <$> split
            t :: FC f c -> FC f a
            t = contramap toC
         in (\a -> t . fc . toC $ a)
  conquer :: FC f a
  conquer = FC empty
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
-- hoist :: Functor g => (forall a. f a -> g a) -> FC f a -> FC g a
-- hoist f (FC cfa) = FC $ fmap (hoist f) . f . cfa
-- count :: FC (Compose [] ((,) Int)) ()
-- count = FC $ \() -> Compose [(1, count), (2, count)]
