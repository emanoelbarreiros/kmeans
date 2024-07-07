module Kmeans where

import qualified Data.List as L
import qualified Data.Map as M
import Lib

compareDistance :: (Vectorizable e v1, Vectorizable e v2) => e -> v1 -> v2 -> Ordering
compareDistance p x y = compare (distance x $ toVector p) (distance y $ toVector p)


clusterAssignmentPhase :: Vectorizable e v => [v] -> [e] -> M.Map v [e]
clusterAssignmentPhase centroids points = let initialMap = M.fromList $ zip centroids (repeat [])
                                          in foldr (\p m -> let chosenC = L.minimumBy (compareDistance p) centroids in M.adjust (p:) chosenC m) initialMap points


clusterAssignmentPhase' :: Vectorizable a1 a2 => [a2] -> [a1] -> M.Map a2 [a1]
clusterAssignmentPhase' centroids points = foldr updateMap initialMap points
                                           where 
                                               initialMap = M.fromList $ zip centroids (repeat [])
                                               updateMap p m = M.adjust (p:) chosenC m
                                                   where chosenC = L.minimumBy (compareDistance p) centroids


newCentroidPhase :: Vectorizable e v => M.Map v [e] -> [(v,v)]
newCentroidPhase = M.toList . fmap (centroid . map toVector)


shouldStop :: (Vector v) => [(v,v)] -> Float -> Bool
shouldStop centroids threshold = foldr (\(x,y) s -> s + distance x y) 0.0 centroids < threshold

-- initialization function
-- number of centroids
-- the information
-- threshold
-- final centroids
kMeans :: Vectorizable e v => (Int -> [e] -> [v]) -> Int -> [e] -> Float -> [v]
kMeans i k points = kMeans' (i k points) points


kMeans' :: Vectorizable e v => [v] -> [e] -> Float -> [v]
kMeans' centroids points threshold =
  let assignments     = clusterAssignmentPhase centroids points
      oldNewCentroids = newCentroidPhase assignments
      newCentroids    = map snd oldNewCentroids
  in if shouldStop oldNewCentroids threshold
      then newCentroids
      else kMeans' newCentroids points threshold


initializeSimple :: Int -> [e] -> [(Float, Float)]
initializeSimple 0 _ = []
initializeSimple n v = (fromIntegral n, fromIntegral n) : initializeSimple (n-1) v