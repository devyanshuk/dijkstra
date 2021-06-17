module Deps.Graph 
(
    Graph(..),
    Weight,
    Neighbor(..),
    parseGraph,
    outgoingEdge,
    allVertices,
    allEdges,
    nodeInfo
)
where


import Data.List
import Text.Read(readMaybe)
import qualified Data.Map as Map

type Weight = Float

data Neighbor a = Neighbor
            {
                item :: a,
                weight :: Weight
            }
            deriving (Show, Eq, Ord)

data Graph a = Graph (Map.Map a [Neighbor a])
    deriving(Show, Eq, Ord)

{- input types -}
type RawInputLine = [String] -- [vertex1, vertex2, weight]
type ParsedInputLine a = ((a, a), Weight) -- ((vertex1, vertex2), weight)



{- Given a list of ((from, to), weight), convert it into a graph type -}
parseToGraph :: [((String, String), Weight)]
                -> Map.Map String [Neighbor String]

parseToGraph parsedLines = Map.fromList $ map (\n -> (n, outgoingEdges n parsedLines)) sources
                        where
                            sources = nub $ map (fst . fst) parsedLines

                            outgoingEdges :: String -> [((String, String), Weight)] -> [Neighbor String]
                            outgoingEdges node parsedLines = map (\((_, vertex2), weight) -> Neighbor vertex2 weight) allNodes
                                                where
                                                    allNodes = filter (\((vertex1, _), _) -> node == vertex1) parsedLines




{- Vertex1 Vertex2 Weight [Comments/Extra info will be ignored] -}
parseLine :: [String]
             -> ((String, String), Weight)

parseLine (vertex1:vertex2:weight:_) = case parsedWeight of
                                        Nothing -> error "Parse error"
                                        Just _weight -> ((vertex1, vertex2), _weight)
                                    where
                                        parsedWeight = readMaybe weight :: Maybe Weight


    
{- given a string (node1 node2 weight)*, parse it into graph -}
parseGraph :: [String] -> Graph String
parseGraph [] = Graph (Map.empty)
parseGraph inputLines = Graph $ parseToGraph parsed
                    where
                        parsed = map (parseLine . words) $ filter (\x -> x /= []) inputLines
    



{- Given a graph and a vertex, return the vertex and a list of all its neighbors -}
nodeInfo :: (Eq a, Ord a)
            => Graph a
            -> a {- node to find information for -}
            -> Maybe(a, [Neighbor a])

nodeInfo (Graph g) vertex | Map.null g = Nothing
                          | otherwise = case item of
                                            Nothing -> Nothing
                                            (Just neighborList) -> Just(vertex, neighborList)
                          where
                              item = Map.lookup vertex g



{- all neighbors of a node in the graph -}
outgoingEdge :: (Eq a, Ord a)
                => Graph a
                -> a {- vertex to find outgoing edges for -}
                -> [Neighbor a]

outgoingEdge graph@(Graph g) vertex | Map.null g = []
                                    | otherwise = case info of
                                                    Nothing -> []
                                                    (Just (_, neighbors)) -> neighbors
                                    where
                                      info = nodeInfo graph vertex


                                
{- [Neighbor { item = "a", weight = 12.0}, Neighbor { item = "b", weight = 1.0}]  -> ["a", "b"]   -}
nodeFor :: (Eq a, Ord a)
            => [Neighbor a]
            -> [a]
nodeFor = map item



{- [Neighbor { item = "a", weight = 12.0}, Neighbor { item = "b", weight = 1.0}]  -> [12.0, 1.0]   -}
weightFor :: (Eq a, Ord a)
            => [Neighbor a]
            -> [Weight]
weightFor = map weight



{- return all vertices of a graph. -}
allVertices :: (Eq a, Ord a)
               => Graph a
               -> [a]

allVertices (Graph g) | Map.null g = []
                      | otherwise = nub $ foldr (\key acc -> key : (nodeFor $ g Map.! key) ++ acc) [] $ Map.keys g



{- return all edges of a graph. Format : ((from, to), weight) -}
allEdges :: (Eq a, Ord a)
            => Graph a
            -> [((a, a), Weight)] {- [((from, to), weight)] -}

allEdges (Graph g) | Map.null g = []
                   | otherwise = [((source, _item), _weight) |  source <- Map.keys g,
                                                                neighbor <- g Map.! source,
                                                                let _item = item neighbor,
                                                                let _weight = weight neighbor]

    

    