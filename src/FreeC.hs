{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module FreeC
  (
  ) where

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
  divide split (FC cvb) (FC cvc) =
    FC . pure $ \a ->
      let (b, c) = split a
          FC bbb = contramap (fst . split) (FC cvb)
          FC ccc = contramap (snd . split) (FC cvc)
       in FC (bbb <|> ccc)
  conquer :: FC f a
  conquer = FC empty

instance Alternative f => Decidable (FC f) where
  lose :: (a -> Void) -> FC f a
  lose f = FC (pure (absurd . f))
  choose :: forall a b c. (a -> Either b c) -> FC f b -> FC f c -> FC f a
  choose choice (FC fb) (FC fc) = FC $ liftA2 makeAChoice fb fc
    where
      makeAChoice :: (b -> FC f b) -> (c -> FC f c) -> (a -> FC f a)
      makeAChoice fb' fc' a =
        case choice a of
          Left b -> choose choice (fb' b) (FC fc)
          Right c -> choose choice (FC fb) (fc' c)

step :: (Functor f) => a -> FC f a -> f (FC f a)
step a (FC fa) = fmap ($ a) fa

joinStep :: Monad m => a -> FC m a -> FC m a
joinStep a (FC m) =
  FC $ do
    fa <- m
    runFC $ fa a

extractStep :: Comonad w => a -> FC w a -> FC w a
extractStep a (FC wa) = extract wa a

failTrue :: FC Maybe Bool
failTrue =
  FC $
  Just $ \case
    True -> FC Nothing
    False -> failTrue

hoist :: Functor f => (forall a. f a -> g a) -> FC f a -> FC g a
hoist f (FC cfa) = FC $ f (fmap (hoist f) <$> cfa)

failNegative :: FC (Either String) Int
failNegative = contramap (< 0) $ hoist (toEither "negative") failTrue

toEither :: String -> Maybe a -> Either String a
toEither e Nothing = Left e
toEither _ (Just r) = Right r

failEven :: FC (Either String) Int
failEven = contramap even $ hoist (toEither "even") failTrue

dup :: a -> (a, a)
dup a = (a, a)

combo :: FC (Either String) Int
combo = divide dup failNegative failEven
