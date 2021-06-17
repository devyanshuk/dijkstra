import Deps.Graph
import Deps.Dijkstra
import Data.Char(isSpace)
import Data.List(dropWhile, intercalate)
import qualified Data.Map as Map

loop :: [String] {- accumulator -}
        -> IO [String]

loop acc = do
    currLine <- getLine
    let trimmedLine = dropWhile isSpace currLine
    if trimmedLine == [] then return acc
    else loop $ trimmedLine : acc



{-
        Reads lines from the console and produces a String Graph
-}
fromConsole :: IO (Graph String)
fromConsole = do
        putStrLn "Format : [vertex1] [vertex2] [edge]. Press Enter to stop giving input"
        input <- loop []
        return $ parseGraph input



{-
        Reads lines from a file and produces a String Graph
-}
fromFile :: String {- input filename -}
            -> IO (Graph String)

fromFile inputFile = do
        contents <- readFile inputFile
        return $ parseGraph $ lines contents



{-
    Given a graph and a start vertex v,
    display shortest paths from v to all other vertices in the graph
    Format : 
-}
displayShortestPathsFrom :: (Eq a, Show a, Ord a)
                            => Graph a
                            -> a
                            -> IO()

displayShortestPathsFrom graph@(Graph g) start =
        if Map.null g then do
                return ()
        else do
                let result@(costs, _) = dijkstra graph start       
                let costsList = toList costs
                displayPath costsList result



displayPath :: (Eq a, Show a, Ord a)
               =>[(a, Weight)]
               -> DijkstraResult a
               -> IO()

displayPath [] _ = do return ()
displayPath ((destinationNode, distanceFromSource) : rest) result = do
        let path = reverse $ getPathTo destinationNode result
        putStrLn $ (show destinationNode) ++ " : " ++ (displayList path) ++ " Distance : " ++ (show distanceFromSource)
        displayPath rest result



displayList :: Show a => [a] -> String
displayList [] = ""
displayList [x] = show x
displayList (x:xs) = (show x) ++ " -> " ++ displayList xs