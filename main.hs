import Deps.Graph
import Deps.Dijkstra
import Data.Char(isSpace)
import Data.List(dropWhile, intercalate)

loop :: [String] {- accumulator -}
        -> IO [String]

loop acc = do
    currLine <- getLine
    let trimmedLine = dropWhile isSpace currLine
    if trimmedLine == [] then return acc
    else loop $ trimmedLine : acc



fromConsole :: IO (Graph String)
fromConsole = do
        putStrLn "Format : [vertex1] [vertex2] [edge]. Press Enter to stop giving input"
        input <- loop []
        return $ parseGraph input



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

displayShortestPathsFrom Null _ = do return ()
displayShortestPathsFrom graph start = do
    let result = dijkstra graph start
    case result of
        Nothing -> do
                return ()

        (Just (costs, _)) -> do            
                let costsList = toList costs
                displayPath costsList result


displayPath :: (Eq a, Show a, Ord a)
               =>[(a, Weight)]
               -> Maybe (DijkstraResult a)
               -> IO()

displayPath [] _ = do return ()
displayPath ((destinationNode, distanceFromSource) : rest) result = do
        let path = getPathTo destinationNode result
        putStrLn $ (show destinationNode) ++ " : " ++ (displayList path) ++ " Distance : " ++ (show distanceFromSource)
        displayPath rest result

        

displayList :: Show a => [a] -> String
displayList [] = ""
displayList [x] = show x
displayList (x:xs) = (show x) ++ " -> " ++ displayList xs