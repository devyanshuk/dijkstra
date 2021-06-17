module Deps.Dijkstra 
(
    dijkstra,
    getPathTo,
    DijkstraResult,
    toList
)
where

import Deps.Graph
import Data.Tuple(swap)
import qualified Data.Set as Set
import qualified Data.Map as Map

{-((path cost, previous vertex of a node in the shortest path), priority queue) -}
type QueueState a = ((Map.Map a Weight, Map.Map a a), Set.Set (Weight, a)) 

{- (path cost, previous vertex of a node in the shortest path) -}
type DijkstraResult a = (Map.Map a Weight, Map.Map a a)



toList = Map.toList


{- 
    sets distance (from the source vertex) for the source vertex to 0
    and the remaining vertices of the graph to infinity
-}
initializeSingleSource :: (Eq a, Show a)
                          => a {- source vertex -}
                          -> [a] {- all vertices of the graph -}
                          -> [(Weight, a)] {- (distance from source, node) -}

initializeSingleSource source vertices
    | source `elem` vertices =
            [ (if v == source then 0.0 else 1.0/0.0, v) | v <- vertices ]

    | otherwise = error $ show source ++ " : vertex not present in graph"



{- 
    given a graph and a source vertex,
    return a list of shortest path to all other vertices
-}
dijkstra :: (Eq a, Show a, Ord a)
            => Graph a
            -> a {- source vertex -}
            -> DijkstraResult a

dijkstra graph source = dijkstra' graph initialState visited
                    where
                        vertices = allVertices graph
                        initDistance = initializeSingleSource source vertices
                        priorityQueue = Set.fromList initDistance
                        pathCost = Map.fromList $ map swap initDistance
                        visited = Set.empty
                        prev = Map.empty
                        initialState = ((pathCost, prev), priorityQueue)



{-
    1) find the node(n) with smallest priority
    2) for each v in edges of Graph[n], if priority(n) + length(n, v) < distanceFromSource(v),
        update the path cost of v to (priority(n) + length(n, v)) and previous node of v to n
    3) recursively follow (1) and (2) until the priority queue is empty
-}
dijkstra' :: (Eq a, Show a, Ord a)
             => Graph a
             -> QueueState a
             -> Set.Set a {- explored set -}
             -> DijkstraResult a

dijkstra' graph ((costs, prev), queue) visited
    | Set.null queue = (costs, prev)
    | otherwise = dijkstra' graph newState visited'
    where
        ((smallestPrio, nodeWithSmallestPrio), queue') = Set.deleteFindMin queue
        unvisitedNeighbors = filter 
                                (\neighbor ->
                                        (item neighbor) `Set.notMember` visited 
                                ) $ outgoingEdge graph nodeWithSmallestPrio
        initialState = ((costs, prev), queue')
        newState = 
            foldl (updatePathPrevAndQueue nodeWithSmallestPrio smallestPrio) initialState unvisitedNeighbors
        visited' = Set.insert nodeWithSmallestPrio visited



{-
    Step 2 of the aforementioned algorithm
-}
updatePathPrevAndQueue :: (Eq a, Show a, Ord a)
                            => a {- Node n -}
                            -> Weight {- distance from source of node n -}
                            -> QueueState a {- ((current cost, currnet prev), current queue) -}
                            -> Neighbor a {- neighbor of node n -}
                            -> QueueState a {- ((updated cost, updated prev), updated queue) -}

updatePathPrevAndQueue baseNode baseWeight currState@((costs, prev), queue) neighbor
    | newWeight >= prevWeight = currState
    | otherwise =  ( 
                       (Map.insert vert newWeight costs, Map.insert vert baseNode prev),
                        Set.insert (newWeight, vert) $ Set.delete (prevWeight, vert) queue
                    )
    where
        newWeight = baseWeight + (weight neighbor)
        vert = item neighbor
        prevWeight = costs Map.! vert



{-
    Given results for dijkstra's algorithm,
    return the shortest path to a node (in reversed order)
-}
getPathTo :: (Eq a, Show a, Ord a)
             => a {- destination vertex -}
             -> DijkstraResult a {- result of Dijkstra's algorithm -}
             -> [a]

getPathTo destination result@(costs, prev)
    | distanceFromSource == (1.0/0.0) = []
    | distanceFromSource == 0.0 = [destination]
    | otherwise = destination : (getPathTo (prev Map.! destination) result)
    where
        distanceFromSource = costs Map.! destination             