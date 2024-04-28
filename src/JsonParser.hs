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

parseLinkUrl :: Parser String
parseLinkUrl = skipAll *> parseChar '\"' *> parseChar 'l' *> parseChar 'i' *>
    parseChar 'n' *> parseChar 'k' <* parseChar '\"' *> skipAll *>
    parseChar ':' *> skipAll *> parseChar '{' *> skipAll *>
    skipAll *> parseChar '\"' *> parseChar 'u' *> parseChar 'r' *>
    parseChar 'l' <* parseChar '\"' *> skipAll *> parseChar ':' *> skipAll *>
    parseStringQuoted <* skipAll

parseLinkContent :: Parser String
parseLinkContent = skipAll *> parseChar ',' *>
    skipAll *> parseChar '\"' *> parseChar 'c' *> parseChar 'o' *>
    parseChar 'n' *> parseChar 't' *> parseChar 'e' *> parseChar 'n' *>
    parseChar 't' <* parseChar '\"' *> skipAll *> parseChar ':' *> skipAll *>
    parseChar '[' *> skipAll *> parseStringQuoted <* skipAll <* parseChar ']'
    <* skipAll <* parseChar '}'

parserJsonLink :: Parser ParserValue
parserJsonLink = ParserLink <$> parseLinkUrl <*> parseLinkContent

parseImageUrl :: Parser String
parseImageUrl = skipAll *> parseChar '\"' *> parseChar 'i' *> parseChar 'm' *>
    parseChar 'a' *> parseChar 'g' *> parseChar 'e' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '{' *> skipAll *>
    skipAll *> parseChar '\"' *> parseChar 'u' *> parseChar 'r' *>
    parseChar 'l' <* parseChar '\"' *> skipAll *> parseChar ':' *> skipAll *>
    parseStringQuoted <* skipAll

parseImageAlt :: Parser String
parseImageAlt = skipAll *> parseChar ',' *> skipAll *>
    parseChar '\"' *> parseChar 'a' *> parseChar 'l' *> parseChar 't' *>
    parseChar '\"' *> skipAll *> parseChar ':' *> skipAll *> parseChar '['
    *> skipAll *> parseStringQuoted <* skipAll <* parseChar ']' <* skipAll <*
    parseChar '}'

parserJsonImage :: Parser ParserValue
parserJsonImage = ParserImage <$> parseImageUrl <*> parseImageAlt

parserJsonParagraph :: Parser ParserValue
parserJsonParagraph = ParserParagraph <$> (skipAll *> parseChar '[' *>
    skipAll *> parseCommaSeparated parseJsonValue <* skipAll
    <* parseChar ']')

parserJsonList :: Parser ParserValue
parserJsonList = ParserList <$> (skipAll *> parseChar '\"' *> parseChar 'l' *>
    parseChar 'i' *> parseChar 's' *> parseChar 't' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '[' *> skipAll
    *> parseCommaSeparated parseJsonValue <* skipAll <* parseChar ']')

parserJsonCodeBlock :: Parser ParserValue
parserJsonCodeBlock = ParserCodeBlock <$> (skipAll *> parseChar '\"' *>
    parseChar 'c' *> parseChar 'o' *> parseChar 'd' *> parseChar 'e' *>
    parseChar 'b' *> parseChar 'l' *> parseChar 'o' *> parseChar 'c' *>
    parseChar 'k' *> parseChar '\"' *> skipAll *> parseChar ':' *> skipAll *>
    parseChar '[' *> skipAll *> parseCommaSeparated parseJsonValue <* skipAll
    <* parseChar ']' <* skipAll)

parseSectionTitle :: Parser String
parseSectionTitle = skipAll *> parseChar '\"' *>
    parseChar 's' *> parseChar 'e' *> parseChar 'c' *> parseChar 't' *>
    parseChar 'i' *> parseChar 'o' *> parseChar 'n' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '{' *>
    skipAll *> parseChar '\"' *> parseChar 't' *> parseChar 'i' *>
    parseChar 't' *> parseChar 'l' *> parseChar 'e' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseStringQuoted <* skipAll

parseContentSection :: Parser [ParserValue]
parseContentSection = skipAll *> parseChar ',' *> skipAll *> parseChar '\"' *>
    parseChar 'c' *> parseChar 'o' *> parseChar 'n' *> parseChar 't' *>
    parseChar 'e' *> parseChar 'n' *> parseChar 't' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '[' *> skipAll *>
    parseCommaSeparated parseJsonValue <* skipAll <* parseChar ']' <* skipAll
    <* skipAll <* parseChar '}'

parserJsonSection :: Parser ParserValue
parserJsonSection = ParserSection <$> parseSectionTitle <*> parseContentSection

parserJsonBody :: Parser ParserValue
parserJsonBody = ParserBody <$> (skipAll *> parseChar '\"' *> parseChar 'b' *>
    parseChar 'o' *> parseChar 'd' *> parseChar 'y' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '[' *> skipAll
    *> parseCommaSeparated parseJsonValue <* skipAll <* parseChar ']')

parseDate :: Parser (Maybe String)
parseDate = (Just <$> date) <|> pure Nothing
    where
        date = skipAll *> parseChar '\"' *> parseChar 'd' *>
            parseChar 'a' *> parseChar 't' *> parseChar 'e' <*
            parseChar '\"' *> skipAll *> parseChar ':' *> skipAll *>
            parseString <* skipAll

parseAuthor :: Parser (Maybe String)
parseAuthor = (Just <$> author) <|> pure Nothing
    where
        author = skipAll *> parseChar '\"' *> parseChar 'a' *>
            parseChar 'u' *> parseChar 't' *> parseChar 'h' *> parseChar 'o' *>
            parseChar 'r' <* parseChar '\"' *> skipAll *>
            parseChar ':' *> skipAll *> parseString <* skipAll <* parseChar ','

parseHeaderTitle :: Parser String
parseHeaderTitle = skipAll *> parseChar '\"' *> parseChar 'h' *> parseChar 'e'
    *> parseChar 'a' *> parseChar 'd' *> parseChar 'e' *> parseChar 'r' *>
    parseChar '\"' *> skipAll *> parseChar ':' *> skipAll *> parseChar '{'
    *> skipAll *> parseChar '\"' *> parseChar 't' *> parseChar 'i' *>
    parseChar 't' *> parseChar 'l' *> parseChar 'e' <* parseChar '\"' *>
    skipAll *> parseChar ':' *> skipAll *> parseStringQuoted <* skipAll

parserHeaderAuthor :: Parser (Maybe String)
parserHeaderAuthor = skipAll *> parseChar ',' *> parseAuthor

parserHeaderDate :: Parser (Maybe String)
parserHeaderDate = skipAll *> parseDate <* skipAll
    <* parseChar '}'

parserJsonHeader :: Parser ParserValue
parserJsonHeader = ParserHead <$> parseHeaderTitle <*> parserHeaderAuthor
    <*> parserHeaderDate

parseItalic :: Parser String
parseItalic = skipAll *> parseChar '{' *> skipAll *> parseChar '\"' *>
    parseChar 'i' *> parseChar 't' *> parseChar 'a' *> parseChar 'l' *>
    parseChar 'i' *> parseChar 'c' <* parseChar '\"' *> skipAll *>
    parseChar ':' *> skipAll *> parseString <* skipAll <* parseChar '}'
    <* skipAll

parserJsonItalic :: Parser ParserValue
parserJsonItalic = ParserItalic <$> parseItalic

parseBold :: Parser String
parseBold = skipAll *> parseChar '{' *> skipAll *> parseChar '\"' *>
    parseChar 'b' *> parseChar 'o'  *> parseChar 'l' *> parseChar 'd' <*
    parseChar '\"' *> skipAll *> parseChar ':' *> skipAll *> parseString <*
    skipAll <* parseChar '}' <* skipAll

parserJsonBold :: Parser ParserValue
parserJsonBold = ParserBold <$> parseBold

parseCode :: Parser String
parseCode = skipAll *> parseChar '{' *> skipAll *> parseChar '\"' *>
    parseChar 'c' *> parseChar 'o' *> parseChar 'd' *> parseChar 'e' <*
    parseChar '\"' *> skipAll *> parseChar ':' *> skipAll *> parseString <*
    skipAll <* parseChar '}' <* skipAll

parserJsonCode :: Parser ParserValue
parserJsonCode = ParserCode <$> parseCode

-- Parse JSON string value
parseJsonString :: Parser ParserValue
parseJsonString = ParserString <$> parseString <* skipAll

parserJsonObject :: Parser ParserValue
parserJsonObject = ParserObject <$> (skipAll *> parseChar '{' *> skipAll *>
    parseCommaSeparated parseJsonValue <* skipAll <* parseChar '}')

-- Complete JSON value parser
parseJsonValue :: Parser ParserValue
parseJsonValue = skipAll *> parserJsonObject <|> parserJsonHeader <|>
    parserJsonBody <|>  parserJsonCode <|> parserJsonBold <|> parserJsonItalic
    <|> parserJsonSection <|> parserJsonCodeBlock <|> parserJsonList <|>
    parserJsonParagraph <|> parserJsonImage <|> parserJsonLink
    <|> parseJsonString <|> parseJsonArray

