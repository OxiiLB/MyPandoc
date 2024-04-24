module Lib
    ( runParser
    , parseJsonValue
    , parseString
    ) where

import Control.Applicative (Alternative(..))
import Parser
import Control.Monad (void)

-- Define your JsonValue data type
data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show)

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
    skipAll *> parseCommaSeparated parseJsonValue  <* skipAll
    <* parseChar ']'

-- Parse JSON array value
parseJsonArray :: Parser JsonValue
parseJsonArray = JsonArray <$> createJsonArray

-- Parse JSON string value
parseJsonString :: Parser JsonValue
parseJsonString = JsonString <$> parseString

-- Complete JSON value parser
parseJsonValue :: Parser JsonValue
parseJsonValue = skipAll *> parseJsonArray <|> parseJsonString

skipAll :: Parser ()
skipAll = void $ many $ parseAnyChar " \n\r\t"

-- Parse comma separated values
parseCommaSeparated :: Parser a -> Parser [a]
parseCommaSeparated p =
  (:) <$> p <*> many (skipAll *> parseChar ',' *> skipAll *> p) <|> pure []