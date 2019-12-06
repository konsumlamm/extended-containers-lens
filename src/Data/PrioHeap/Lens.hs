{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.PrioHeap.Lens () where

import Control.Lens.Each
import Control.Lens.Empty
import Control.Lens.Indexed
import Control.Lens.Prism (nearly)
import Control.Lens.Traversal
-- import Control.Lens.Wrapped

import qualified Data.PrioHeap as H
import Data.PrioHeap (PrioHeap)


instance (c ~ d) => Each (PrioHeap c a) (PrioHeap d b) a b where
    each = traversed

instance AsEmpty (PrioHeap k a) where
    _Empty = nearly H.empty null

instance FunctorWithIndex k (PrioHeap k) where
    imap = H.mapWithKey

instance FoldableWithIndex k (PrioHeap k) where
    ifoldMap = H.foldMapWithKey
    ifoldr = H.foldrWithKey
    ifoldl f = H.foldlWithKey (flip f)
    ifoldr' = H.foldrWithKey'
    ifoldl' f = H.foldlWithKey' (flip f)

instance TraversableWithIndex k (PrioHeap k) where
    itraverse = H.traverseWithKey

instance Ord k => TraverseMin k (PrioHeap k) where
    traverseMin f heap = case H.lookupMin heap of
        Nothing -> pure heap
        Just (key, x) -> (\x -> H.adjustMin (const x) heap) <$> indexed f key x
