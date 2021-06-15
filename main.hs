import Graph
import Data.Char(isSpace)
import Data.List(dropWhile)
import System.Environment(getArgs)

loop :: [String] -> IO [String]
loop acc = do
    currLine <- getLine
    let trimmedLine = dropWhile isSpace currLine
    if trimmedLine == [] then return $ reverse acc
    else loop $ trimmedLine : acc

fromConsole :: IO (Graph String)
fromConsole = do
        putStrLn "Format : [vertex1] [vertex2] [edge]. Press Enter to stop giving input"
        input <- loop []
        return $ parseGraph input

fromFile :: String -> IO (Graph String)
fromFile inputFile = do
        contents <- readFile inputFile
        return $ parseGraph $ lines contents