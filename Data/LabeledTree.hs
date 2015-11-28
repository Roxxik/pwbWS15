{-# LANGUAGE TypeOperators #-}
module Data.LabeledTree(
    Tree(..), Forest,
    -- * Building trees
    flattenTreeL, flattenForestL,
    unfoldTree, unfoldForest,
    reduceTree
    ) where

import Data.Traversable
import Data.Foldable
import Control.Monad
import Control.Applicative
import Control.DeepSeq (NFData(rnf))

data Tree k a = Node  {
      rootLabel :: a, -- ^ label value
      subForest :: Forest k a -- ^ zero or more child trees
} deriving (Show, Eq, Read)

-- | label + value pair
data k ::> a = k ::> a
 deriving (Show, Eq, Read)

type Forest k a = [k ::> Tree k a]

instance Functor ((::>) k) where
    fmap = fmapDefault

instance Foldable ((::>) k) where
    foldMap = foldMapDefault

instance Traversable ((::>) k) where
    traverse f (k ::> v) = (::>) k <$> f v

instance Monoid k => Applicative ((::>) k) where
    pure x = mempty ::> x
    k ::> f <*> l ::> x = k `mappend` l ::> f x

instance (NFData k, NFData a) => NFData ((::>) k a) where
    rnf (k ::> v) = rnf k `seq` rnf v



instance Functor (Tree k) where
    fmap = fmapDefault

instance Foldable (Tree k) where
    foldMap = foldMapDefault

instance Traversable (Tree k) where
    traverse f (Node l sf) = Node <$> f l <*> (traverse (traverse (traverse f)) sf)

instance (NFData k, NFData a) => NFData (Tree k a) where
    rnf (Node x ts) = rnf x `seq` rnf ts


-- | The elements of a tree in pre-order.
flattenTreeL :: Monoid k => Tree k a -> [(k, a)]
flattenTreeL = flattenL mempty

-- | The elements of a forest in pre-order.
flattenForestL :: Forest k a -> [(k, a)]
flattenForestL = concatMap (uncurry' flattenL)

flattenL :: k -> Tree k a -> [(k, a)]
flattenL l t = (l, root) : subF
    where
        (root, subF) = reduceTree (,) g [] t
        g l (r, s) b = (l, r) : s ++ b

-- | Build a tree from a seed value
unfoldTree :: (b -> (a, [(k, b)])) -> b -> Tree k a
unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)

-- | Build a forest from a list of seed values
unfoldForest :: (b -> (a, [(k, b)])) -> [(k, b)] -> Forest k a
unfoldForest f = map (\(a,b) -> a ::> unfoldTree f b)


uncurry' :: (a -> b -> c) -> (a ::> b) -> c
uncurry' f (a ::> b) = f a b

reduceTree :: (a -> b -> c) -> (k -> c -> b -> b) -> b -> Tree k a -> c
reduceTree f g z t = f (rootLabel t) (foldr (uncurry' g . fmap (reduceTree f g z)) z (subForest t))
