{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Lib
--}

module Lib
    ( runParser
    , parseJsonValue
    , parseString
    ) where

import Control.Applicative (Alternative(..))
import Parser
import Control.Monad (void)

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
    skipAll *> parseCommaSeparated parseJsonValue <* skipAll
    <* parseChar ']'

-- Parse JSON array value
parseJsonArray :: Parser JsonValue
parseJsonArray = JsonArray <$> createJsonArray

-- Parse JSON string value
parseJsonString :: Parser JsonValue
parseJsonString = JsonString <$> parseString <* skipAll

parseJsonObject :: Parser JsonValue
parseJsonObject = JsonObject <$> (parseChar '{' *> skipAll *>
    parseCommaSeparated ((,) <$> parseString <* skipAll <* parseChar ':'
    <* skipAll <*> parseJsonValue) <* skipAll
    <* parseChar '}')

-- Complete JSON value parser
parseJsonValue :: Parser JsonValue
parseJsonValue = skipAll *> parseJsonArray <|> parseJsonString <|>
    parseJsonObject


skipAll :: Parser ()
skipAll = void $ many $ parseAnyChar " \n\r\t"

-- Parse comma separated values
parseCommaSeparated :: Parser a -> Parser [a]
parseCommaSeparated p =
  (:) <$> p <*> many (skipAll *> parseChar ',' *> skipAll *> p) <|> pure []