{-# LANGUAGE ScopedTypeVariables #-}

module Data.KdTree
    ( KdTree
      -- * Construction
    , fromVector
      -- * Queries
    , nearest
    , toList
      -- * Diagnostics
    , isValid
    ) where

import Prelude hiding (sort)
import Data.List (minimumBy)
import Data.Maybe (maybeToList)
import Data.Ord (comparing)

import Linear hiding (point)
import Control.Lens
import qualified Data.Vector.Generic as V
import Data.Vector.Algorithms.Intro (sortBy)

-- | The k-d tree is a data structure capable of efficiently answering
-- nearest neighbor search queries in low-dimensional spaces. As a rule
-- of thumb, for efficient lookups the number of points in @k@ dimensions
-- should greatly exceed @2^k@
data KdTree f a = KdNode { point :: !(f a)
                         , axis  :: E f
                         , left  :: KdTree f a
                         , right :: KdTree f a
                         }
                | KdEmpty

-- | Construct a @KdTree@ from a vector of points
fromVector :: (Ord a, V.Vector v (f a)) => [E f] -> v (f a) -> KdTree f a
fromVector basis pts = go (cycle basis) pts
  where
    go _ pts | V.null pts = KdEmpty
    go (axis:rest) pts =
      let pts' = V.modify (sortBy $ comparing (^. el axis)) pts
          pivotIdx = V.length pts' `div` 2
      in KdNode { point = pts' V.! pivotIdx
                , axis  = axis
                , left  = go rest $ V.take pivotIdx pts'
                , right = go rest $ V.drop (pivotIdx+1) pts'
                }

quadranceTo :: (Num a, Metric f) => f a -> f a -> a
quadranceTo a b = quadrance (a ^-^ b)

-- | Find the point nearest to the given point
nearest :: forall f a. (Ord a, Num a, Metric f)
        => f a -> KdTree f a -> Maybe (f a)
nearest pt tree = go tree
  where
    go :: KdTree f a -> Maybe (f a)
    go KdEmpty = Nothing
    go (KdNode nodePt axis l r)
      | (pt ^. el axis) <= (nodePt ^. el axis) = go' nodePt axis l r
      | otherwise                              = go' nodePt axis r l

    go' :: f a   -- ^ The point of the node we are sitting at
        -> E f   -- ^ The splitting axis of the node
        -> KdTree f a -- ^ The subnode the query point sits in
        -> KdTree f a -- ^ The other subnode
        -> Maybe (f a)
    go' nodePt axis side other =
      let best = case go side of
                   Nothing    -> [nodePt]
                   Just best' -> [best', nodePt]
          tryAdj = (pt^.el axis - nodePt^.el axis)^2 <= quadrance (pt ^-^ nodePt)
          bestAdj = if tryAdj
                      then [] --maybeToList $ go other
                      else []
      in Just $ minimumBy (comparing $ quadranceTo pt) (best ++ bestAdj)

-- | List all points in a tree
toList :: KdTree f a -> [f a]
toList KdEmpty = []
toList (KdNode point _ l r) = point : (toList l ++ toList r)

-- | Verify that the node is well-formed
nodeIsValid :: Ord a => KdTree f a -> Bool
nodeIsValid KdEmpty = True
nodeIsValid (KdNode point axis l r) =
       all (\p->p^.el axis <= point^.el axis) (toList l)
    && all (\p->p^.el axis >  point^.el axis) (toList r)

-- | Verify that the tree is well-formed (recursively)
isValid :: Ord a => KdTree f a -> Bool
isValid KdEmpty = True
isValid node@(KdNode _ _ l r) =
    nodeIsValid node && isValid l && isValid r

onAxis :: E f -> (a -> a -> b) -> f a -> f a -> b
onAxis (E l) f a b = f (a ^. l) (b ^. l)
