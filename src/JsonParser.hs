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

createJsonArray :: Parser [ParserValue]
createJsonArray = parseChar '[' *>
    skipAll *> parseCommaSeparated parseJsonValue <* skipAll
    <* parseChar ']'

parseJsonArray :: Parser ParserValue
parseJsonArray = ParserArray <$> createJsonArray

parseLink :: Parser (String, ParserValue)
parseLink = parseChar '{' *> skipAll *>
    parseChar '\"' *> parseChar 'l' *> parseChar 'i' *> parseChar 'n' *>
    parseChar 'k' <* parseChar '\"' *> skipAll *>
    parseChar ':' *> skipAll *> parseChar '{' *> skipAll *>
    ((,) <$> parseString <* skipAll <* parseChar ':' <* skipAll <*>
    parseJsonValue) <* skipAll <* parseChar '}' <* skipAll <* parseChar '}'

parserJsonLink :: Parser ParserValue
parserJsonLink = ParserLink <$> parseLink

parseImage :: Parser (String, ParserValue)
parseImage = parseChar '{' *> skipAll *>
    parseChar '\"' *> parseChar 'i' *> parseChar 'm' *> parseChar 'a' *>
    parseChar 'g' *> parseChar 'e' <* parseChar '\"' *> skipAll *>
    parseChar ':' *> skipAll *> parseChar '{' *> skipAll *>
    ((,) <$> parseString <* skipAll <* parseChar ':' <* skipAll <*>
    parseJsonValue) <* skipAll <* parseChar '}' <* skipAll <* parseChar '}'

parserJsonImage :: Parser ParserValue
parserJsonImage = ParserImage <$> parseImage

parseParagraphe :: Parser [ParserValue]
parseParagraphe = skipAll *> parseChar '{' *> skipAll *> parseChar '\"' *>
    parseChar 'p' *> parseChar 'a' *> parseChar 'r' *> parseChar 'a' *>
    parseChar 'g' *> parseChar 'r' *> parseChar 'a' *> parseChar 'p' *>
    parseChar 'h' *> parseChar 'e' *> parseChar '\"' *> skipAll *>
    parseChar ':' *> skipAll *> parseChar '[' *> skipAll *>
    parseCommaSeparated parseJsonValue <* skipAll <* parseChar ']' <* skipAll

parserJsonParagraphe :: Parser ParserValue
parserJsonParagraphe = ParserParagraphe <$> parseParagraphe

parseList :: Parser [ParserValue]
parseList = skipAll *> parseChar '{' *> skipAll *> parseChar '\"' *> parseChar 'l' *>
    parseChar 'i' *> parseChar 's' *> parseChar 't' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '[' *> skipAll *>
    parseCommaSeparated parseJsonValue <* skipAll <* parseChar ']' <* skipAll

parserJsonList :: Parser ParserValue
parserJsonList = ParserList <$> parseList

parseCodeBlock :: Parser [ParserValue]
parseCodeBlock = skipAll *> parseChar '{' *> skipAll *> parseChar '\"' *> parseChar 'c' *>
    parseChar 'o' *> parseChar 'd' *> parseChar 'e' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '[' *> skipAll *>
    parseCommaSeparated parseJsonValue <* skipAll <* parseChar ']' <* skipAll

parserJsonCodeBlock :: Parser ParserValue
parserJsonCodeBlock = ParserCodeBlock <$> parseCodeBlock

parseSection :: Parser [ParserValue]
parseSection = skipAll *> parseChar '{' *> skipAll *> parseChar '\"' *> parseChar 's' *>
    parseChar 'e' *> parseChar 'c' *> parseChar 't' *> parseChar 'i' *>
    parseChar 'o' *> parseChar 'n' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '[' *> skipAll *>
    parseCommaSeparated parseJsonValue <* skipAll <* parseChar ']' <* skipAll

parserJsonSection :: Parser ParserValue
parserJsonSection = ParserSection <$> parseSection

parseBody :: Parser [ParserValue]
parseBody = skipAll *> parseChar '{' *> skipAll *> parseChar '\"' *> parseChar 'b' *>
    parseChar 'o' *> parseChar 'd' *> parseChar 'y' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '[' *> skipAll *>
    parseCommaSeparated parseJsonValue <* skipAll <* parseChar ']' <* skipAll

parserJsonBody :: Parser ParserValue
parserJsonBody = ParserBody <$> parseBody

parseHeader :: Parser [ParserValue]
parseHeader = skipAll *> parseChar '{' *> skipAll *> parseChar '\"' *> parseChar 'h' *>
    parseChar 'e' *> parseChar 'a' *> parseChar 'd' *> parseChar 'e' *> parseChar 'r' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '[' *> skipAll *>
    parseCommaSeparated parseJsonValue <* skipAll <* parseChar ']' <* skipAll

parserJsonHeader :: Parser ParserValue
parserJsonHeader = ParserHead <$> parseHeader

parseItalic :: Parser String
parseItalic = skipAll *> parseChar '{' *> skipAll *> parseChar '\"' *> parseChar 'i' *>
    parseChar 't' *> parseChar 'a' *> parseChar 'l' *> parseChar 'i' *>
    parseChar 'c' <* parseChar '\"' *> skipAll *> parseChar ':' *> skipAll *>
    parseString <* skipAll <* parseChar '}' <* skipAll

parserJsonItalic :: Parser ParserValue
parserJsonItalic = ParserItalic <$> parseItalic

parseBold :: Parser String
parseBold = skipAll *> parseChar '{' *> skipAll *> parseChar '\"' *> parseChar 'b' *>
    parseChar 'o'  *> parseChar 'l' *> parseChar 'd' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseString <* skipAll <* parseChar '}'
    <* skipAll

parserJsonBold :: Parser ParserValue
parserJsonBold = ParserBold <$> parseBold

parseCode :: Parser String
parseCode = skipAll *> parseChar '{' *> skipAll *> parseChar '\"' *> parseChar 'c' *>
    parseChar 'o' *> parseChar 'd' *> parseChar 'e' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseString <* skipAll <* parseChar '}'
    <* skipAll

parserJsonCode :: Parser ParserValue
parserJsonCode = ParserCode <$> parseCode

-- Parse JSON string value
parseJsonString :: Parser ParserValue
parseJsonString = ParserString <$> parseString <* skipAll

parserJsonObject :: Parser ParserValue
parserJsonObject = ParserObject <$> (parseChar '{' *> skipAll *>
    parseCommaSeparated ((,) <$> parseString <* skipAll <* parseChar ':'
    <* skipAll <*> parseJsonValue) <* skipAll
    <* parseChar '}')

-- Complete JSON value parser
parseJsonValue :: Parser ParserValue
parseJsonValue = skipAll *> parserJsonHeader <|> parserJsonBody <|> 
    parserJsonCode <|> parserJsonBold <|> parserJsonItalic
    <|> parserJsonSection <|> parserJsonCodeBlock <|> parserJsonList <|>
    parserJsonParagraphe <|> parserJsonImage <|> parserJsonLink
    <|> parseJsonString <|> parseJsonArray <|> parserJsonObject

skipAll :: Parser ()
skipAll = void $ many $ parseAnyChar " \n\r\t"

-- Parse comma separated values
parseCommaSeparated :: Parser a -> Parser [a]
parseCommaSeparated p =
  (:) <$> p <*> many (skipAll *> parseChar ',' *> skipAll *> p) <|> pure []