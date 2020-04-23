{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.PrioHeap.Lens
    ( prioHeapOf
    ) where

import Data.Functor ((<&>))

import Control.Lens.Combinators (IndexedGetting, iviews)
import Control.Lens.Each (Each(..))
import Control.Lens.Empty (AsEmpty(..))
import Control.Lens.Indexed
import Control.Lens.Iso (iso)
import Control.Lens.Prism (nearly)
import Control.Lens.Traversal
import Control.Lens.Wrapped

import qualified Data.PrioHeap as H
import Data.PrioHeap (PrioHeap)

instance (c ~ d) => Each (PrioHeap c a) (PrioHeap d b) a b where
    each = traversed
    {-# INLINE each #-}

instance AsEmpty (PrioHeap k a) where
    _Empty = nearly H.empty null

instance FunctorWithIndex k (PrioHeap k) where
    imap = H.mapWithKey
    {-# INLINE imap #-}

instance FoldableWithIndex k (PrioHeap k) where
    ifoldMap = H.foldMapWithKey
    {-# INLINE ifoldMap #-}

    ifoldr = H.foldrWithKey
    {-# INLINE ifoldr #-}

    ifoldl f = H.foldlWithKey (flip f)
    {-# INLINE ifoldl #-}

    ifoldr' = H.foldrWithKey'
    {-# INLINE ifoldr' #-}

    ifoldl' f = H.foldlWithKey' (flip f)
    {-# INLINE ifoldl' #-}


instance TraversableWithIndex k (PrioHeap k) where
    itraverse = H.traverseWithKey
    {-# INLINE itraverse #-}

instance Ord k => TraverseMin k (PrioHeap k) where
    traverseMin f heap = case H.lookupMin heap of
        Nothing -> pure heap
        Just (key, x) -> indexed f key x <&> \y -> H.adjustMin (const y) heap

-- TODO: TraverseMax instance for PrioHeap (Down k)?

instance Ord k => Wrapped (PrioHeap k a) where
    type Unwrapped (PrioHeap k a) = [(k, a)]

    _Wrapped' = iso H.toList H.fromList
    {-# INLINE _Wrapped' #-}

-- | Use @'wrapping' 'H.fromList'@. Unwrapping returns some permutation of the list.
instance (t ~ PrioHeap k' a', Ord k) => Rewrapped (PrioHeap k a) t

prioHeapOf :: IndexedGetting k (PrioHeap k a) s a -> s -> PrioHeap k a
prioHeapOf l = iviews l H.singleton
