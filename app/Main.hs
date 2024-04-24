{--
-- EPITECH PROJECT, 2024
-- main
-- File description:
-- Main function of the project
--}

module Main (main) where
import System.Environment (getArgs)
import Lib

-- Read JSON content from file
readJSONFile :: FilePath -> IO String
readJSONFile filePath = readFile filePath

-- Main function to test the JSON parser
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            putStrLn $ "Parsing the JSON file: " ++ filePath
            jsonContent <- readJSONFile filePath
            putStrLn "Read the file contents:"
            putStrLn jsonContent
            putStrLn "Parsing the JSON text..."
            case runParser parseJsonValue jsonContent of
                Just (parsedJson, remaining) -> do
                    putStrLn "Parsed result:"
                    print parsedJson
                    putStrLn "Remaining input:"
                    putStrLn remaining
                Nothing -> putStrLn "Failed to parse JSON"
        _ -> putStrLn "Please provide the path to the JSON file as a command-line argument"
