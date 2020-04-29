{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Heap.Lens
    ( heapOf
    ) where

import Data.Foldable (toList)

import Control.Lens.Combinators (Getting, views)
import Control.Lens.Empty (AsEmpty(..))
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

-- | Construct a 'Heap' from a 'Getter', 'Control.Lens.Fold.Fold', 'Control.Lens.Traversal.Traversal', 'Control.Lens.Lens.Lens' or 'Control.Lens.Iso.Iso'.
heapOf :: Getting (Heap a) s a -> s -> Heap a
heapOf l = views l H.singleton
