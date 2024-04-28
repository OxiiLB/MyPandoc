{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- XmlParser
--}

module XmlParser
    (
        parseXmlValue
    ) where

import Control.Applicative (Alternative(..))
import Parser
import Data.List ()
import Debug.Trace (trace)

parseString :: Parser String
parseString = many (parseAnyChar
            (['a' .. 'z'] ++ ['A' .. 'Z'] ++ " " ++ ['0' .. '9'] ++ "-"
               ++ "." ++ "_" ++ "/" ++ "\\" ++ ":" ++ "@" ++ "*" ++ "&"
               ++ "%" ++ "+" ++ "=" ++ "!" ++ "?" ++ "#" ++ "$" ++ "^"
               ++ "(" ++ ")" ++ "[" ++ "]" ++ "{" ++ "}" ++
            "," ++ ";" ++ "'" ++ "`" ++ "~" ++ "|" ++ " "))

-- Parse date in header of the Xml file
parseDate :: Parser (Maybe String)
parseDate = do
    _ <- skipAll
    _ <- parseStr "<date>"
    date <- parseString
    _ <- parseStr "</date>"
    _ <- skipAll
    return $ Just date

-- Parse author in header of the Xml file
parseAuthor :: Parser (Maybe String)
parseAuthor = do
    _ <- skipAll
    _ <- parseStr "<author>"
    author <- parseString
    _ <- parseStr "</author>"
    return $ Just author

skipEndHeader :: Parser ()
skipEndHeader = do
    _ <- skipAll
    _ <- parseStr "</header>"
    _ <- skipAll
    return ()

-- Parse header of the Xml file
parseXmlHeader :: Parser ParserValue
parseXmlHeader = do
    _ <- skipAll
    _ <- parseStr "<header title="
    title <- parseStringQuoted
    _ <- parseChar '>'
    author <- parseAuthor
    date <- parseDate
    _ <- skipEndHeader
    return $ ParserHead title author date

-- Parse code in the body of the Xml file
parseXmlCode :: Parser ParserValue
parseXmlCode = do
    _ <- skipAll
    _ <- parseStr "<code>"
    _ <- skipAll
    code <- parseString
    _ <- skipAll
    _ <- parseStr "</code>"
    _ <- skipAll
    return $ ParserCode code

-- Parse italic in the body of the Xml file
parseXmlItalic :: Parser ParserValue
parseXmlItalic = do
    _ <- skipAll
    _ <- parseStr "<italic>"
    _ <- skipAll
    italic <- parseString
    _ <- skipAll
    _ <- parseStr "</italic>"
    _ <- skipAll
    return $ ParserItalic italic

-- Parse bold in the body of the Xml file
parseXmlBold :: Parser ParserValue
parseXmlBold = do
    _ <- skipAll
    _ <- parseStr "<bold>"
    _ <- skipAll
    bold <- parseString
    _ <- skipAll
    _ <- parseStr "</bold>"
    _ <- skipAll
    return $ ParserBold bold

parseXmlImageUrl :: Parser String
parseXmlImageUrl = skipAll *> parseStr "<image url=" *> skipAll *>
    parseStringQuoted <* skipAll <* parseChar '>'

parseXmlImageAlt :: Parser String
parseXmlImageAlt = skipAll *> parseString <* skipAll <* parseStr "</image>"
    <* skipAll

parseXmlImage :: Parser ParserValue
parseXmlImage = ParserImage <$> parseXmlImageUrl <*> parseXmlImageAlt

parseXmlLinkUrl :: Parser String
parseXmlLinkUrl = skipAll *> parseStr "<link url=" *> skipAll *>
    parseStringQuoted <* skipAll <* parseChar '>'

parseXmlLinkContent :: Parser String
parseXmlLinkContent = skipAll *> parseString <* skipAll <* parseStr "</link>"
    <* skipAll

parseXmlLink :: Parser ParserValue
parseXmlLink = ParserLink <$> parseXmlLinkUrl <*> parseXmlLinkContent

parseXmlParagraph :: Parser ParserValue
parseXmlParagraph = do
    _ <- skipAll
    _ <- parseStr "<paragraph>"
    _ <- skipAll
    paragraph <- manyTill parseXmlValue (parseStr "</paragraph>")
    _ <- skipAll
    return $ ParserParagraph paragraph

parseXmlCodeBlock :: Parser ParserValue
parseXmlCodeBlock = do
    _ <- parseStr "<codeblock>"
    _ <- skipAll
    codeBlock <- manyTill parseXmlValue (parseStr "</codeblock>")
    _ <- skipAll
    return $ ParserCodeBlock codeBlock

parseXmlList :: Parser ParserValue
parseXmlList = do
    _ <- parseStr "<list>"
    _ <- skipAll
    list <- manyTill parseXmlValue (parseStr "</list>")
    _ <- skipAll
    return $ ParserList list

parseXmlSectionTitle :: Parser String
parseXmlSectionTitle = skipAll *> parseStr "<section title=" *> skipAll *>
    parseStringQuoted <* skipAll <* parseChar '>'

parseXmlSectionContent :: Parser [ParserValue]
parseXmlSectionContent = skipAll *> manyTill parseXmlValue
    (parseStr "</section>") <* skipAll

parseXmlSection :: Parser ParserValue
parseXmlSection = ParserSection <$> parseXmlSectionTitle <*>
    parseXmlSectionContent

parseXmlBody :: Parser ParserValue
parseXmlBody = do
    _ <- parseStr "<body>"
    _ <- skipAll
    body <- manyTill parseXmlValue (parseStr "</body>")
    _ <- skipAll
    return $ ParserBody body

parseXmlString :: Parser ParserValue
parseXmlString = ParserString <$> parseString <* skipAll

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = scan
  where
    scan = (end *> pure []) <|> ((:) <$> p <*> scan)

parseXmlObject :: Parser ParserValue
parseXmlObject = do
    _ <- parseStr "<document>"
    _ <- skipAll
    pairs <- manyTill parseXmlValue (parseStr "</document>")
    _ <- skipAll
    return $ ParserObject pairs

-- Complete Xml value parser
parseXmlValue :: Parser ParserValue
parseXmlValue = skipAll *> parseXmlObject <|> parseXmlHeader <|> parseXmlBody
    <|> parseXmlParagraph <|> parseXmlSection <|> parseXmlBold <|>
    parseXmlItalic <|> parseXmlCode <|> parseXmlCodeBlock <|> parseXmlList <|>
    parseXmlLink <|> parseXmlImage <|> parseXmlString
