{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- JsonParser
--}

module JsonParser
    ( runParser
    , parseJsonValue
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

-- createJsonArray :: Parser [ParserValue]
-- createJsonArray = parseChar '[' *>
--     skipAll *> parseCommaSeparated parseJsonValue <* skipAll
--     <* parseChar ']'

-- parseJsonArray :: Parser ParserValue
-- parseJsonArray = ParserArray <$> createJsonArray

parseItalic :: Parser String
parseItalic = parseChar '{' *> skipAll *> parseChar '\"' *> parseChar 'i' *>
    parseChar 't' *> parseChar 'a' *> parseChar 'l' *> parseChar 'i' *>
    parseChar 'c' <* parseChar '\"' *> skipAll *> parseChar ':' *> skipAll *>
    parseString <* skipAll <* parseChar '}' <* skipAll

parserJsonItalic :: Parser ParserValue
parserJsonItalic = ParserItalic <$> parseItalic

parseBold :: Parser String
parseBold = parseChar '{' *> skipAll *> parseChar '\"' *> parseChar 'b' *>
    parseChar 'o'  *> parseChar 'l' *> parseChar 'd' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseString <* skipAll <* parseChar '}'
    <* skipAll

parserJsonBold :: Parser ParserValue
parserJsonBold = ParserBold <$> parseBold

parseCode :: Parser String
parseCode = parseChar '{' *> skipAll *> parseChar '\"' *> parseChar 'c' *>
    parseChar 'o' *> parseChar 'd' *> parseChar 'e' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseString <* skipAll <* parseChar '}'
    <* skipAll

parserJsonCode :: Parser ParserValue
parserJsonCode = ParserCode <$> parseCode

-- Parse JSON string value
parseJsonString :: Parser ParserValue
parseJsonString = ParserString <$> parseString <* skipAll

parseJsonObject :: Parser ParserValue
parseJsonObject = ParserObject <$> (parseChar '{' *> skipAll *>
    parseCommaSeparated ((,) <$> parseString <* skipAll <* parseChar ':'
    <* skipAll <*> parseJsonValue) <* skipAll
    <* parseChar '}')

-- Complete JSON value parser
parseJsonValue :: Parser ParserValue
parseJsonValue = skipAll *> parseJsonString <|> parserJsonItalic <|> 
    parserJsonCode <|> parserJsonBold <|> parseJsonObject

skipAll :: Parser ()
skipAll = void $ many $ parseAnyChar " \n\r\t"

-- Parse comma separated values
parseCommaSeparated :: Parser a -> Parser [a]
parseCommaSeparated p =
  (:) <$> p <*> many (skipAll *> parseChar ',' *> skipAll *> p) <|> pure []