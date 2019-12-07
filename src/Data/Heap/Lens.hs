{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Heap.Lens () where

import Data.Foldable (toList)

import Control.Lens.Empty
import Control.Lens.Iso (iso)
import Control.Lens.Prism (nearly)
import Control.Lens.Wrapped

import qualified Data.Heap as H
import Data.Heap (Heap)

instance AsEmpty (Heap a) where
    _Empty = nearly H.empty null

instance Ord a => Wrapped (Heap a) where
    type Unwrapped (Heap a) = [a]

    _Wrapped' = iso toList H.fromList
    {-# INLINE _Wrapped' #-}

-- | Use @'wrapping' 'H.fromList'@. Unwrapping returns some permutation of the list.
instance (t ~ Heap a', Ord a) => Rewrapped (Heap a) t
