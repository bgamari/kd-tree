{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.KdTree
    ( KdTree
      -- * Construction
    , fromVector
      -- * Queries
    , nearest
    , points
    , toList
      -- * Diagnostics
    , isValid
    , showKdTree
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
-- should greatly exceed @2^k@.
--
-- The average complexity of a nearest neighbor lookup is logarithmic in
-- the number of points although can be linear in the worst case.
data KdTree ann f a = KdNode { point :: !(f a)
                             , axis  :: E f
                             , ann   :: ann
                             , left  :: KdTree ann f a
                             , right :: KdTree ann f a
                             }
                    | KdEmpty

-- | Construct a @KdTree@ from a vector of points
fromVector :: (Ord a, V.Vector v (f a, ann))
           => [E f] -> v (f a, ann) -> KdTree ann f a
fromVector basis pts = go (cycle basis) pts
  where
    go _ pts | V.null pts = KdEmpty
    go (axis:rest) pts =
      let pts' = V.modify (sortBy $ comparing (^. _1 . el axis)) pts
          pivotIdx = V.length pts' `div` 2
          pivot = pts' V.! pivotIdx
      in KdNode { point = pivot ^. _1
                , axis  = axis
                , ann   = pivot ^. _2
                , left  = go rest $ V.take pivotIdx pts'
                , right = go rest $ V.drop (pivotIdx+1) pts'
                }

quadranceTo :: (Num a, Metric f) => f a -> f a -> a
quadranceTo a b = quadrance (a ^-^ b)

-- | Find the point nearest to the given point. On average this has
-- complexity logarithmic in the number of points.
nearest :: forall ann f a. (Ord a, Num a, Metric f)
        => f a -> KdTree ann f a -> Maybe (f a, ann)
nearest pt tree = go tree
  where
    go :: KdTree ann f a -> Maybe (f a, ann)
    go KdEmpty = Nothing
    go (KdNode nodePt axis ann l r)
      | (pt ^. el axis) <= (nodePt ^. el axis) = go' nodePt axis ann l r
      | otherwise                              = go' nodePt axis ann r l

    go' :: f a   -- ^ The point of the node we are sitting at
        -> E f   -- ^ The splitting axis of the node
        -> ann
        -> KdTree ann f a -- ^ The subnode the query point sits in
        -> KdTree ann f a -- ^ The other subnode
        -> Maybe (f a, ann)
    go' nodePt axis ann side other =
      let best = case go side of
                   Nothing    -> [(nodePt, ann)]
                   Just best' -> [best', (nodePt, ann)]
          tryAdj = (pt^.el axis - nodePt^.el axis)^2 <= quadrance (pt ^-^ nodePt)
          bestAdj = if tryAdj
                      then maybeToList $ go other
                      else []
          dist a = quadrance $ pt ^-^ fst a
      in Just $ minimumBy (comparing dist) (best ++ bestAdj)

-- | List all points and values in a tree
toList :: KdTree ann f a -> [(f a, ann)]
toList KdEmpty = []
toList (KdNode point _ ann l r) = (point, ann) : (toList l ++ toList r)

-- | List all points in a tree
points :: KdTree ann f a -> [f a]
points = map fst . toList

-- | Verify that the node is well-formed
nodeIsValid :: Ord a => KdTree ann f a -> Bool
nodeIsValid KdEmpty = True
nodeIsValid (KdNode point axis _ l r) =
       all (\p->p^.el axis <= point^.el axis) (points l)
    && all (\p->p^.el axis >  point^.el axis) (points r)

-- | Verify that the tree is well-formed (recursively)
isValid :: Ord a => KdTree ann f a -> Bool
isValid KdEmpty = True
isValid node@(KdNode _ _ _ l r) =
    nodeIsValid node && isValid l && isValid r

onAxis :: E f -> (a -> a -> b) -> f a -> f a -> b
onAxis (E l) f a b = f (a ^. l) (b ^. l)

-- | Given names for the axes show the tree
showKdTree :: Show (f a) => f String -> KdTree ann f a -> String
showKdTree axisNames tree = unlines $ fmt 0 tree
  where
    --fmt :: Int -> Kdtree f a -> [String]
    fmt depth node =
      case node of
        KdEmpty -> [indent "KdEmpty"]
        (KdNode point axis _ l r) ->
          [ indent $ "KdNode ("++show point++") "++show (axisNames ^. el axis) ]
          ++ fmt (depth+2) l
          ++ [""]
          ++ fmt (depth+2) r
      where indent = (replicate depth ' ' ++)
