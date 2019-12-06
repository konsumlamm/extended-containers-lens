module Data.Heap.Lens () where

import Control.Lens.Empty
import Control.Lens.Prism (nearly)
-- import Control.Lens.Wrapped

import qualified Data.Heap as H
import Data.Heap (Heap)

instance AsEmpty (Heap a) where
    _Empty = nearly H.empty null
