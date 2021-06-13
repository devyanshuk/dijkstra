import Data.List
import System.Environment(getArgs)

type Weight = Float
data Neighbor a = Neighbor {
                item :: a,
                weight :: Weight
            }
            deriving (Show, Eq)

data Graph a = Null | Graph [(a, [Neighbor a])]
    deriving(Show, Eq)

{- input types -}
type RawInputLine = [String] -- [vertex1, vertex2, weight]
type ParsedInputLine a = ((a, a), Weight) -- ((vertex1, vertex2), weight)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = nub

parseToGraph :: [ParsedInputLine String] -> Graph String
parseToGraph parsedLines = Graph (map (\n -> (n, outgoingEdges n)) nodes)
            where
                firstElementofParsedInputLine = (fst . fst) -- ((Node1, Node2) Weight) -> Node1
                nodes = removeDuplicates $ map firstElementofParsedInputLine $ parsedLines

                outgoingEdges :: String -> [Neighbor String]
                outgoingEdges node = map (\((_, vertex2), weight) -> Neighbor vertex2 weight) allNodes
                                  where
                                      allNodes = filter (\((vertex1, _), _) -> node == vertex1) parsedLines

{- Vertex1 Vertex2 Weight [Comments/Extra info will be ignored] -}
parseLine :: RawInputLine -> ParsedInputLine String
parseLine (vertex1:vertex2:weight:_) = ((vertex1, vertex2), read weight :: Weight)

{- given a string (node1 node2 weight)*, parse it into graph -}
parseGraph :: String -> Graph String
parseGraph [] = Null
parseGraph inputLines = parseToGraph parsed
                    where
                        parsed = map (parseLine . words) $ lines inputLines

{- Given a graph and a vertex, return the vertex and a list of all its neighbors -}
nodeInfo :: Eq a => Graph a -> a -> Maybe(a, [Neighbor a])
nodeInfo Null _ = Nothing
nodeInfo (Graph g) vertex | info == [] = Nothing
                          | otherwise = Just $ head info
                          where
                            info = filter (\(vertex1, _) -> vertex1 == vertex) g

{- all neighbors of a node in the graph -}
outgoingEdge :: Eq a => Graph a -> a -> [Neighbor a]
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
            
main :: IO (Graph String)
main = do
    inputFile <- getArgs

    case inputFile of
        [] -> do
            putStrLn "No argument provided. Program will read graph from the command line. Format : [vertex1] [vertex2] [edge]. Press Ctrl + D to produce results"
            streamInput <- getContents
            putStrLn ""
            return $ parseGraph streamInput

        _ -> do
            contents <- readFile $ head inputFile
            return $ parseGraph contents


    