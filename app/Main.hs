{--
-- EPITECH PROJECT, 2024
-- main
-- File description:
-- Main function of the project
--}

module Main (main) where
import System.Environment (getArgs)
import JsonParser
import ErrorHandling (checkArgs)
import GetInfo(getInputFile, getInfoArgs)
import Lib(parseFile, defaultInfo)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

-- Read JSON content from file
readJSONFile :: FilePath -> IO String
readJSONFile filePath = readFile filePath

usage :: IO ()
usage = putStrLn "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]\
                \\n\n\tifile\tpath to the file to convert\n\toformat\toutput \
                \format (xml, json, markdown)\n\tofile\tpath to the output \
                \file\n\tiformat\tinput format (xml, json, markdown)"

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
