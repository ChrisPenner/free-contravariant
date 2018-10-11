{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}

module Ana where

import Data.Functor.Foldable
import FreeContravariant

newtype FCF f a r = FCF
  { runFCF :: (a -> f r)
  } deriving (Functor)

type instance Base (FC f a) = FCF f a

instance (Functor f) => Corecursive (FC f a) where
  embed (FCF afr) = FC afr

counter :: FC [] Int
counter = ana go 1
  where
    go n = FCF $ \i -> replicate (i + n) 1
    -- go n = FCF $ \i -> [i + (n - 1), i + (n - 2), i + (n - 3)]
