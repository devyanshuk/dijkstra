module Deps.Dijkstra 
(
    dijkstra
)
where

import Deps.Graph
import qualified Data.Set as Set
import qualified Data.Map as Map

{-
type ShortestPath a = [a]

data Distance a = PositiveInfinity | Distance a
    deriving(Show, Eq)

instance Ord a => Ord (Distance a) where
    compare PositiveInfinity PositiveInfinity = EQ
    compare PositiveInfinity _ = GT
    compare _ PositiveInfinity = LT
    compare (Distance x) (Distance y) = compare x y

-}


{- sets distance (from the source vertex) for the source vertex to 0
   and the remaining vertices of the graph to infinity -}

initializeSingleSource :: (Eq a, Show a)
                          => Graph a
                          -> a {- source vertex -}
                          -> [a] {- all vertices of the graph -}
                          -> [(Weight, a)] {- (distance from source, (node, previous)) -}

initializeSingleSource Null _ _ = []
initializeSingleSource graph@(Graph g) source vertices
    | source `elem` vertices =
            foldr (\v acc ->
                        if v == source then (0.0, v):acc
                        else (1.0/0.0, v):acc
                  ) [] vertices
    | otherwise = error $ (show source) ++ " : vertex not present in graph"



{- given a graph and a source vertex, return a list of shortest path to all other vertices -}
dijkstra :: (Eq a, Show a, Ord a)
            => Graph a
            -> a {- source vertex -}
            -> Maybe(Map.Map a Weight, Map.Map a a)

dijkstra Null _ = Nothing
dijkstra graph source = _dijkstra graph priorityQueue pathCost Map.empty visited
                    where
                        vertices = allVertices graph
                        initDistance = initializeSingleSource graph source vertices
                        priorityQueue = Set.fromList initDistance
                        pathCost = Map.fromList $ map (\(x, y) -> (y, x)) initDistance
                        visited = Set.empty



{-
    1) find the node(n) with smallest priority
    2) for each v in edges of Graph[n], if priority(n) + length(n, v) < distanceFromSource(v),
        update the path cost of v to (priority(n) + length(n, v)) and previous node of v to n
    3) recursively follow (1) and (2) until the priority queue is empty
-}
_dijkstra :: (Eq a, Show a, Ord a)
             => Graph a
             -> Set.Set (Weight, a) {- priority queue -}
             -> Map.Map a Weight {- path cost of each vertex from source -}
             -> Map.Map a a {- previous 'list' -}
             -> Set.Set a {- set of unvisited vertices -}
             -> Maybe(Map.Map a Weight, Map.Map a a)

_dijkstra Null _ _ _ _ = Nothing
_dijkstra graph queue costs prev visited
    | Set.null queue = Just(costs, prev)
    | otherwise = _dijkstra graph queue'' costs' prev' visited'
    where
        ((smallestPrio, nodeWithSmallestPrio), queue') = Set.deleteFindMin queue
        unvisitedNeighbors = filter (\neighbor ->
                                        (item neighbor) `Set.notMember` visited 
                                    ) $ outgoingEdge graph nodeWithSmallestPrio
        ((costs', prev'), queue'') = updatePathPrevAndQueue nodeWithSmallestPrio smallestPrio unvisitedNeighbors costs prev queue'
        visited' = Set.insert nodeWithSmallestPrio visited


updatePathPrevAndQueue :: (Eq a, Show a, Ord a)
                            => a {- Node n -}
                            -> Weight {- distance from source of node n -}
                            -> [Neighbor a] {- unvisited outgoing edges of node n -}
                            -> Map.Map a Weight {- path cost of all vertices -}
                            -> Map.Map a a {- previous list -}
                            -> Set.Set (Weight, a)
                            -> ((Map.Map a Weight, Map.Map a a), Set.Set (Weight, a))

updatePathPrevAndQueue _ _ [] costs prev queue = ((costs, prev), queue)
updatePathPrevAndQueue baseNode baseWeight (n : neighbors) costs prev queue
    | newWeight >= prevWeight = 
                    updatePathPrevAndQueue baseNode
                                           baseWeight
                                           neighbors
                                           costs
                                           prev
                                           queue
    | otherwise =
        updatePathPrevAndQueue baseNode
                               baseWeight
                               neighbors
                               (Map.insert vert newWeight costs)
                               (Map.insert vert baseNode prev)
                               (Set.insert (newWeight, vert) $ Set.delete (prevWeight, vert) queue)
    where
        newWeight = baseWeight + (weight n)
        vert = item n
        prevWeight = costs Map.! vert
