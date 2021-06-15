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

type Weight = Float

data Neighbor a = Neighbor
            {
                item :: a,
                weight :: Weight
            }
            deriving (Show, Eq)

data Graph a = Null | Graph [(a, [Neighbor a])]
    deriving(Show, Eq)

{- input types -}
type RawInputLine = [String] -- [vertex1, vertex2, weight]
type ParsedInputLine a = ((a, a), Weight) -- ((vertex1, vertex2), weight)


removeDuplicates :: Eq a
                    => [a]
                    -> [a]

removeDuplicates = nub



parseToGraph :: [ParsedInputLine String]
                -> Graph String

parseToGraph parsedLines = Graph (map (\n -> (n, outgoingEdges n)) nodes)
            where
                firstElementofParsedInputLine = (fst . fst) -- ((Node1, Node2) Weight) -> Node1
                nodes = removeDuplicates $ map firstElementofParsedInputLine $ parsedLines

                outgoingEdges :: String -> [Neighbor String]
                outgoingEdges node = map (\((_, vertex2), weight) -> Neighbor vertex2 weight) allNodes
                                  where
                                      allNodes = filter (\((vertex1, _), _) -> node == vertex1) parsedLines



{- Vertex1 Vertex2 Weight [Comments/Extra info will be ignored] -}
parseLine :: RawInputLine
             -> ParsedInputLine String

parseLine (vertex1:vertex2:weight:_) = case parsedWeight of
                                        Nothing -> error "Parse error"
                                        Just _weight -> ((vertex1, vertex2), _weight)
                                    where
                                        parsedWeight = readMaybe weight :: Maybe Weight


    
{- given a string (node1 node2 weight)*, parse it into graph -}
parseGraph :: [String] -> Graph String
parseGraph [] = Null
parseGraph inputLines = parseToGraph parsed
                    where
                        parsed = map (parseLine . words) $ filter (\x -> x /= []) inputLines



{- Given a graph and a vertex, return the vertex and a list of all its neighbors -}
nodeInfo :: Eq a
            => Graph a
            -> a {- node to find information for -}
            -> Maybe(a, [Neighbor a])

nodeInfo Null _ = Nothing
nodeInfo (Graph g) vertex | info == [] = Nothing
                          | otherwise = Just $ head info
                          where
                            info = filter (\(vertex1, _) -> vertex1 == vertex) g



{- all neighbors of a node in the graph -}
outgoingEdge :: Eq a
                => Graph a
                -> a {- vertex to find outgoing edges for -}
                -> [Neighbor a]

outgoingEdge Null _ = []
outgoingEdge graph@(Graph g) vertex = case info of
                                    Nothing -> []
                                    Just info -> snd info
                                    where
                                      info = nodeInfo graph vertex


                                
{- [Neighbor { item = "a", weight = 12.0}, Neighbor { item = "b", weight = 1.0}]  -> ["a", "b"]   -}
nodeFor :: [Neighbor a] -> [a]
nodeFor = map item



{- [Neighbor { item = "a", weight = 12.0}, Neighbor { item = "b", weight = 1.0}]  -> [12.0, 1.0]   -}
weightFor :: [Neighbor a] -> [Weight]
weightFor = map weight



allVertices :: Eq a
               => Graph a
               -> [a]

allVertices Null = []
allVertices (Graph g) = removeDuplicates $ foldr (\(vertex, neighbors) acc -> vertex:((nodeFor neighbors) ++ acc)) [] g



allEdges :: Eq a => Graph a
            -> [((a, a), Weight)] {- [((from, to), weight)] -}

allEdges Null = []
allEdges (Graph g) = [((source, _item), _weight)| (source, neighbors) <- g,
                                                  neighbor <- neighbors,
                                                  let _item = item neighbor,
                                                  let _weight = weight neighbor
                      ]

    

    