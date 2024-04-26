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
import Debug.Trace (trace)
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
parseParagraphe =
    skipAll *> (trace "parseParagraphe\n" $ parseChar '[') *>
    skipAll *>
    parseCommaSeparated parseJsonValue <* skipAll <* parseChar ']'

parserJsonParagraph :: Parser ParserValue
parserJsonParagraph = ParserParagraphe <$> parseParagraphe

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

parserJsonSection :: Parser ParserValue
parserJsonSection = do
    _ <- skipAll
    _ <- parseChar '\"'
    _ <- parseChar 's'
    _ <- parseChar 'e'
    _ <- parseChar 'c'
    _ <- parseChar 't'
    _ <- parseChar 'i'
    _ <- parseChar 'o'
    _ <- parseChar 'n'
    _ <- parseChar '\"'
    _ <- skipAll
    _ <- parseChar ':'
    _ <- skipAll
    _ <- parseChar '{'
    _ <- skipAll
    _ <- parseChar '\"'
    _ <- parseChar 't'
    _ <- parseChar 'i'
    _ <- parseChar 't'
    _ <- parseChar 'l'
    _ <- parseChar 'e'
    _ <- parseChar '\"'
    _ <- skipAll
    _ <- parseChar ':'
    _ <- skipAll
    title <- parseStringQuoted
    _ <- parseChar ','
    _ <- skipAll
    _ <- parseChar '\"'
    _ <- parseChar 'c'
    _ <- parseChar 'o'
    _ <- parseChar 'n'
    _ <- parseChar 't'
    _ <- parseChar 'e'
    _ <- parseChar 'n'
    _ <- parseChar 't'
    _ <- parseChar '\"'
    _ <- skipAll
    _ <- parseChar ':'
    _ <- skipAll
    _ <- parseChar '['
    content <- parseCommaSeparated parseJsonValue
    _ <- skipAll
    _ <- parseChar ']'
    _ <- skipAll
    _ <- parseChar '}'
    return $ ParserSection title content
   
parserJsonBody :: Parser ParserValue
parserJsonBody = do
    _ <- skipAll
    _ <- parseChar '\"'
    _ <- parseChar 'b'
    _ <- parseChar 'o'
    _ <- parseChar 'd'
    _ <- parseChar 'y'
    _ <- parseChar '\"'
    _ <- skipAll
    _ <- parseChar ':'
    _ <- skipAll
    _ <- parseChar '['
    _ <- skipAll
    body <- parseCommaSeparated parseJsonValue
    _ <- skipAll
    _ <- parseChar ']'
    return $ ParserBody body
parseDate :: Parser (Maybe String)
parseDate = (Just <$> date) <|> pure Nothing
    where
        date = skipAll *> parseChar '\"' *> parseChar 'd' *>
            parseChar 'a' *> parseChar 't' *> parseChar 'e' <* parseChar '\"' *>
            skipAll *> parseChar ':' *> skipAll *> parseString <* skipAll

parseAuthor :: Parser (Maybe String)
parseAuthor = (Just <$> author) <|> pure Nothing
    where
        author = skipAll *> parseChar '\"' *> parseChar 'a' *>
            parseChar 'u' *> parseChar 't' *> parseChar 'h' *> parseChar 'o' *>
            parseChar 'r' <* parseChar '\"' *> skipAll *> parseChar ':' *> skipAll *>
            parseString <* skipAll <* parseChar ','

parserJsonHeader :: Parser ParserValue
parserJsonHeader = do
    _ <- skipAll
    _ <- parseChar '\"'
    _ <- parseChar 'h'
    _ <- parseChar 'e'
    _ <- parseChar 'a'
    _ <- parseChar 'd'
    _ <- parseChar 'e'
    _ <- parseChar 'r'
    _ <- parseChar '\"'
    _ <- skipAll
    _ <- parseChar ':'
    _ <- skipAll
    _ <- parseChar '{'
    _ <- skipAll
    _ <- parseChar '\"'
    _ <- parseChar 't'
    _ <- parseChar 'i'
    _ <- parseChar 't'
    _ <- parseChar 'l'
    _ <- parseChar 'e'
    _ <- parseChar '\"'
    _ <- skipAll
    _ <- parseChar ':'
    _ <- skipAll
    title <- parseStringQuoted
    _ <- parseChar ','
    author <- parseAuthor
    date <- parseDate
    _ <- skipAll
    _ <- parseChar '}'
    return $ ParserHead title author date

parseItalic :: Parser String
parseItalic = skipAll *> parseChar '{' *> skipAll *> parseChar '\"' *> parseChar 'i' *>
    parseChar 't' *> parseChar 'a' *> parseChar 'l' *> parseChar 'i' *>
    parseChar 'c' <* parseChar '\"' *> skipAll *> parseChar ':' *> skipAll *>
    parseString <* skipAll <* parseChar '}' <* skipAll

parserJsonItalic :: Parser ParserValue
parserJsonItalic = ParserItalic <$> parseItalic

parseBold :: Parser String
parseBold = skipAll *> parseChar '\"' *> parseChar 'b' *>
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
parserJsonObject = do
  _ <- parseChar '{'
  skipAll
  pairs <- parseCommaSeparated parseJsonValue
  skipAll
  _ <- parseChar '}'
  skipAll
  return $ ParserObject pairs

-- Complete JSON value parser
parseJsonValue :: Parser ParserValue
parseJsonValue = skipAll *> parserJsonObject <|> parserJsonHeader <|>
    parserJsonBody <|>  parserJsonCode <|> parserJsonBold <|> parserJsonItalic
    <|> parserJsonSection <|> parserJsonCodeBlock <|> parserJsonList <|>
    parserJsonParagraph <|> parserJsonImage <|> parserJsonLink
    <|> parseJsonString <|> parseJsonArray

skipAll :: Parser ()
skipAll = void $ many $ parseAnyChar " \n\r\t"

-- Parse comma separated values
parseCommaSeparated :: Parser a -> Parser [a]
parseCommaSeparated p =
  (:) <$> p <*> many (skipAll *> (trace "parseCommaSeparated\n" $ parseChar ',' ) *> skipAll *> p) <|> pure []