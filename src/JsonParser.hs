{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- JsonParser
--}

module JsonParser
    ( runParser
    , parseJsonValue
    , parserJsonSection
    , parserJsonCodeBlock
    , parserJsonList
    , parserJsonParagraph
    , parserJsonImage
    , parserJsonLink
    , parserJsonBody
    , parserJsonHeader
    , parserJsonItalic
    , parserJsonBold
    , parserJsonCode
    , parserJsonObject
    , parseJsonString
    , parseJsonArray
    , parseString
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
parseLinkUrl = skipAll *> parseChar '{' *> skipAll *> parseStr "\"link\"" *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '{' *> skipAll *>
    skipAll *> parseStr "\"url\"" *> skipAll *> parseChar ':' *> skipAll *>
    parseStringQuoted <* skipAll

parseLinkContent :: Parser String
parseLinkContent = skipAll *> parseChar ',' *>
    skipAll *> parseStr "\"content\"" *> skipAll *> parseChar ':' *> skipAll *>
    parseChar '[' *> skipAll *> parseStringQuoted <* skipAll <* parseChar ']'
    <* skipAll <* parseChar '}' <* skipAll <* parseChar '}'

parserJsonLink :: Parser ParserValue
parserJsonLink = ParserLink <$> parseLinkUrl <*> parseLinkContent

parseImageUrl :: Parser String
parseImageUrl = skipAll *> parseChar '{' *> skipAll *> parseStr "\"image\"" *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '{' *> skipAll *>
    skipAll *> parseStr "\"url\"" *> skipAll *> parseChar ':' *> skipAll *>
    parseStringQuoted <* skipAll

parseImageAlt :: Parser String
parseImageAlt = skipAll *> parseChar ',' *> skipAll *>
    parseStr "\"alt\"" *> skipAll *> parseChar ':' *> skipAll *> parseChar '['
    *> skipAll *> parseStringQuoted <* skipAll <* parseChar ']' <* skipAll <*
    parseChar '}' <* skipAll <* parseChar '}'

parserJsonImage :: Parser ParserValue
parserJsonImage = ParserImage <$> parseImageUrl <*> parseImageAlt

parserJsonParagraph :: Parser ParserValue
parserJsonParagraph = ParserParagraph <$> (skipAll *> parseChar '[' *>
    skipAll *> parseCommaSeparated parseJsonValue <* skipAll
    <* parseChar ']')

parserJsonList :: Parser ParserValue
parserJsonList = ParserList <$> (parseChar '{' *> skipAll *>
    parseStr "\"list\"" *> skipAll *> parseChar ':' *> skipAll *> parseChar '['
    *> skipAll *> parseCommaSeparated parseJsonValue <* skipAll <*
    parseChar ']' <* skipAll <* parseChar '}')

parserJsonCodeBlock :: Parser ParserValue
parserJsonCodeBlock = ParserCodeBlock <$> (parseChar '{' *>
    skipAll *> parseStr "\"codeblock\"" *> skipAll *> parseChar ':' *> skipAll
    *> parseChar '[' *> skipAll *> parseCommaSeparated parseJsonValue <*
    skipAll <* parseChar ']' <* skipAll <* parseChar '}')

parseSectionTitle :: Parser String
parseSectionTitle = skipAll *> parseChar '{' *> skipAll *>
    parseStr "\"section\"" *> skipAll *> parseChar ':' *> skipAll *>
    parseChar '{' *> skipAll *> parseStr "\"title\"" *> skipAll *>
    parseChar ':' *> skipAll *> parseStringQuoted <* skipAll

parseContentSection :: Parser [ParserValue]
parseContentSection = skipAll *> parseChar ',' *> skipAll *>
    parseStr "\"content\"" *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '[' *> skipAll *>
    parseCommaSeparated parseJsonValue <* skipAll <* parseChar ']' <* skipAll
    <* skipAll <* parseChar '}' <* skipAll <* parseChar '}'

parserJsonSection :: Parser ParserValue
parserJsonSection = ParserSection <$> parseSectionTitle <*> parseContentSection

parserJsonBody :: Parser ParserValue
parserJsonBody = ParserBody <$> (skipAll *> parseStr "\"body\"" *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '[' *> skipAll
    *> parseCommaSeparated parseJsonValue <* skipAll <* parseChar ']')

parseDate :: Parser (Maybe String)
parseDate = (Just <$> date) <|> pure Nothing
    where
        date = parseChar ',' *> skipAll *> parseStr "\"date\"" *> skipAll *>
            parseChar ':' *> skipAll *> parseString

parseAuthor :: Parser (Maybe String)
parseAuthor = (Just <$> author) <|> pure Nothing
    where
        author = parseChar ',' *> skipAll *> parseStr "\"author\"" *>
            skipAll *> parseChar ':' *> skipAll *> parseString

parseHeaderTitle :: Parser String
parseHeaderTitle = skipAll *> parseStr "\"header\"" *>
    skipAll *> parseChar ':' *> skipAll *> parseChar '{'
    *> skipAll *> parseStr "\"title\"" *>
    skipAll *> parseChar ':' *> skipAll *> parseStringQuoted <* skipAll

parserHeaderAuthor :: Parser (Maybe String)
parserHeaderAuthor = skipAll *> parseAuthor

parserHeaderDate :: Parser (Maybe String)
parserHeaderDate = skipAll *> parseDate

parserJsonHeader :: Parser ParserValue
parserJsonHeader = do
    title <- parseHeaderTitle
    _ <- skipAll
    author <- parserHeaderAuthor
    _ <- skipAll
    date <- parserHeaderDate
    _ <- skipAll
    _ <- parseChar '}'
    return $ ParserHead title author date

parseItalic :: Parser String
parseItalic = skipAll *> parseChar '{' *> skipAll *> parseStr "\"italic\"" *>
    skipAll *> parseChar ':' *> skipAll *> parseString <* skipAll
    <* parseChar '}' <* skipAll

parserJsonItalic :: Parser ParserValue
parserJsonItalic = ParserItalic <$> parseItalic

parseBold :: Parser String
parseBold = skipAll *> parseChar '{' *> skipAll *> parseStr "\"bold\"" *>
    skipAll *> parseChar ':' *> skipAll *> parseString <*
    skipAll <* parseChar '}' <* skipAll

parserJsonBold :: Parser ParserValue
parserJsonBold = ParserBold <$> parseBold

parseCode :: Parser String
parseCode = skipAll *> parseChar '{' *> skipAll *> parseStr "\"code\""
    *> skipAll *> parseChar ':' *> skipAll *> parseString <*
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
parseJsonValue = skipAll *> parserJsonHeader <|>
    parserJsonBody <|>  parserJsonCode <|> parserJsonBold <|> parserJsonItalic
    <|> parserJsonSection <|> parserJsonCodeBlock <|> parserJsonList <|>
    parserJsonParagraph <|> parserJsonImage <|> parserJsonLink
    <|> parseJsonString <|> parseJsonArray <|> parserJsonObject

