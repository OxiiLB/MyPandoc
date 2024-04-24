module Lib
    ( runParser
    , printParsedResult
    , parseJsonValue
    , simpleJsonExample
    , parseString
    ) where

import Control.Applicative (Alternative(..))
import Parser

-- Define your JsonValue data type
data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show)

deleteSpaces :: Parser String
deleteSpaces = parseChar ' ' *> parseString

-- zsefzelndezjdzednlzjed
parseString :: Parser String
parseString = parseChar '"' *> many (parseAnyChar
            (['a' .. 'z'] ++ ['A' .. 'Z'] ++ " " ++ ['0' .. '9'] ++ "-"
               ++ "." ++ "_" ++ "/" ++ "\\" ++ ":" ++ "@" ++ "*" ++ "&"
               ++ "%" ++ "+" ++ "=" ++ "!" ++ "?" ++ "#" ++ "$" ++ "^"
               ++ "(" ++ ")" ++ "[" ++ "]" ++ "{" ++ "}" ++ "<" ++ ">"
               ++ "," ++ ";" ++ "'" ++ "`" ++ "~" ++ "|" ++ " "))
              <* parseChar '"'

createJsonArray :: Parser [JsonValue]
createJsonArray = parseChar '[' *>
    deleteSpaces *> parseJsonValue  <* deleteSpaces
    <* parseChar ']'

-- Parse JSON array value
parseJsonArray :: Parser JsonValue
parseJsonArray = JsonArray <$> createJsonArray

createJsonObject :: Parser [(String, JsonValue)]
createJsonObject = parseChar '{' *> parseString `parseAnd` (parseChar ':' *> parseJsonValue) `parseAnd` many (parseChar ',' *> parseString `parseAnd` (parseChar ':' *> parseJsonValue)) <* parseChar '}'

parseJsonObject :: Parser JsonValue
parseJsonObject = JsonObject <$> createJsonObject

-- Complete JSON value parser
parseJsonValue :: Parser JsonValue
parseJsonValue =  deleteSpaces <|> parseJsonArray <|> parseJsonObject

-- Define simpleJsonExample
simpleJsonExample :: String
simpleJsonExample = "{\"name\":\"John\",\"age\":30,\"city\":\"New York\"}"

skipAll :: Parser ()
skipAll = void $ many $ parseAnyChar " \n\r\t"

-- Parse comma separated values
parseCommaSeparated :: Parser a -> Parser [a]
parseCommaSeparated p =
  (:) <$> p <*> many (skipAll *> parseChar ',' *> skipAll *> p) <|> pure []

-- Define printParsedResult function
printParsedResult :: Show a => Parser a -> IO ()
printParsedResult parser =
    case runParser parser simpleJsonExample of
        Just (jsonValue, remaining) -> do
            putStrLn $ "Parsed result: " ++ show jsonValue
            putStrLn $ "Remaining input: " ++ remaining
        Nothing -> putStrLn "Failed to parse"