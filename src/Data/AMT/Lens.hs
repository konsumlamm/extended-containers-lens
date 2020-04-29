{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Data.AMT.Lens
    ( vectorOf
    ) where

import Data.Foldable (toList)

import Control.Lens.At (Ixed(..), Index, IxValue)
import Control.Lens.Combinators (Getting, views)
import Control.Lens.Cons (Cons(..), Snoc(..))
import Control.Lens.Each (Each(..))
import Control.Lens.Empty (AsEmpty(..))
import Control.Lens.Indexed
import Control.Lens.Iso (Reversing(..), iso)
import Control.Lens.Operators ((<&>))
import Control.Lens.Prism (nearly, prism)
import Control.Lens.Traversal (traversed)
import Control.Lens.Type (Getter, Fold, Traversal, Lens, Iso)
import Control.Lens.Wrapped

import qualified Data.AMT as V
import Data.AMT (Vector)

type instance Index (Vector a) = Int
type instance IxValue (Vector a) = a

instance Ixed (Vector a) where
    ix i f v = case V.lookup i v of
        Nothing -> pure v
        Just x -> f x <&> \y -> V.update i y v

instance Cons (Vector a) (Vector b) a b where
    _Cons = prism (uncurry (V.<|)) $ \v -> case V.viewl v of
        Nothing -> Left V.empty
        Just x -> Right x

instance Snoc (Vector a) (Vector b) a b where
    _Snoc = prism (uncurry (V.|>)) $ \v -> case V.viewr v of
        Nothing -> Left V.empty
        Just x -> Right x

instance Each (Vector a) (Vector b) a b where
    each = traversed
    {-# INLINE each #-}

instance AsEmpty (Vector a) where
    _Empty = nearly V.empty null

instance FunctorWithIndex Int Vector where
    imap = V.mapWithIndex
    {-# INLINE imap #-}

instance FoldableWithIndex Int Vector where
    ifoldMap = V.foldMapWithIndex
    {-# INLINE ifoldMap #-}

    ifoldr = V.foldrWithIndex
    {-# INLINE ifoldr #-}

    ifoldl f = V.foldlWithIndex (flip f)
    {-# INLINE ifoldl #-}

    ifoldr' = V.foldrWithIndex'
    {-# INLINE ifoldr' #-}

    ifoldl' f = V.foldlWithIndex' (flip f)
    {-# INLINE ifoldl' #-}


instance TraversableWithIndex Int Vector where
    itraverse = V.traverseWithIndex
    {-# INLINE itraverse #-}

instance Reversing (Vector a) where
    reversing = V.fromList . reverse . toList

instance Wrapped (Vector a) where
    type Unwrapped (Vector a) = [a]

    _Wrapped' = iso toList V.fromList
    {-# INLINE _Wrapped' #-}

instance (t ~ Vector a') => Rewrapped (Vector a) t

-- | Construct a 'Vector' from a 'Getter', 'Fold', 'Traversal', 'Lens' or 'Iso'.
vectorOf :: Getting (Vector a) s a -> s -> Vector a
vectorOf l = views l V.singleton
