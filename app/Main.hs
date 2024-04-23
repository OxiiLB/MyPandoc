{--
-- EPITECH PROJECT, 2024
-- main
-- File description:
-- Main function of the project
--}

module Main (main) where

import Lib

-- Main function to test the JSON parser
main :: IO ()
main = do
    putStrLn $ "Simple JSON Example: " ++ simpleJsonExample
    putStrLn "Parsing the simple JSON example..."
    case runParser parseJsonValue simpleJsonExample of
        Just (jsonValue, _) -> do
            putStrLn "Parsing successful!"
            putStrLn $ "Parsed result: " ++ show jsonValue
        Nothing -> putStrLn "Failed to parse JSON"