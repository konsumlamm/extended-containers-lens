{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.AMT.Lens () where

import Data.Foldable (toList)

import Control.Lens.At
-- import Control.Lens.Cons
import Control.Lens.Each
import Control.Lens.Empty
import Control.Lens.Indexed
import Control.Lens.Iso
import Control.Lens.Prism (nearly)
import Control.Lens.Traversal (traversed)
-- import Control.Lens.Wrapped

import qualified Data.AMT as V
import Data.AMT (Vector)

type instance Index (Vector a) = Int
type instance IxValue (Vector a) = a

instance Ixed (Vector a) where
    -- ix :: Int -> Traversal' (Vector a) a
    -- ix :: Applicative f => Int -> (a -> f a) -> Vector a -> f (Vector a)
    ix i f v = case V.lookup i v of
        Nothing -> pure v
        Just x -> (\x -> V.update i x v) <$> f x

-- instance Snoc (Vector a) (Vector b) a b

instance Each (Vector a) (Vector b) a b where
    each = traversed

instance AsEmpty (Vector a) where
    _Empty = nearly V.empty null

instance FunctorWithIndex Int Vector where
    imap = V.mapWithIndex

instance FoldableWithIndex Int Vector where
    ifoldMap = V.foldMapWithIndex
    ifoldr = V.foldrWithIndex
    ifoldl f = V.foldlWithIndex (flip f)
    ifoldr' = V.foldrWithIndex'
    ifoldl' f = V.foldlWithIndex' (flip f)

instance TraversableWithIndex Int Vector where
    itraverse = V.traverseWithIndex

instance Reversing (Vector a) where
    reversing = V.fromList . reverse . toList
