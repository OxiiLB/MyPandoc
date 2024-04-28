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
    _ <- trace "skipeEndHeaderBegin\n" skipAll
    _ <- parseStr "</header>"
    _ <- trace "skipeEndHeaderEnd\n" skipAll
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
    _ <- trace "HeaderEnd\n" skipEndHeader
    return $ ParserHead title author date

-- Parse code in the body of the Xml file
parseCode :: Parser ParserValue
parseCode = do
    _ <- skipAll
    _ <- parseStr "<code>"
    _ <- skipAll
    code <- parseString
    _ <- skipAll
    _ <- parseStr "</code>"
    _ <- skipAll
    return $ ParserCode code

-- Parse italic in the body of the Xml file
parseItalic :: Parser ParserValue
parseItalic = do
    _ <- skipAll
    _ <- parseStr "<italic>"
    _ <- skipAll
    italic <- parseString
    _ <- skipAll
    _ <- parseStr "</italic>"
    _ <- skipAll
    return $ ParserItalic italic

-- Parse bold in the body of the Xml file
parseBold :: Parser ParserValue
parseBold = do
    _ <- skipAll
    _ <- parseStr "<bold>"
    _ <- skipAll
    bold <- parseString
    _ <- skipAll
    _ <- parseStr "</bold>"
    _ <- skipAll
    return $ ParserBold bold

-- Parse link in the body of the Xml file
-- parseLink :: Parser ParserValue
-- parseLink = do
--     _ <- skipAll
--     _ <- parseChar '<'
--     _ <- parseChar 'l'
--     _ <- parseChar 'i'
--     _ <- parseChar 'n'
--     _ <- parseChar 'k'
--     _ <- parseChar ' '
--     _ <- parseChar 'u'
--     _ <- parseChar 'r'
--     _ <- parseChar 'l'
--     _ <- parseChar '='
--     link <- parseStringQuoted
--     _ <- parseChar '>'
--     bold <- parseBold
--     italic <- parseItalic
--     code <- parseCode
--     return $ ParserLink (link, ParserParagraph []) --------------------------------- idk

-- Parse image in the body of the Xml file
-- parseImage :: Parser ParserValue
-- parseImage = do
--     _ <- skipAll
--     _ <- parseChar '<'
--     _ <- parseChar 'i'
--     _ <- parseChar 'm'
--     _ <- parseChar 'a'
--     _ <- parseChar 'g'
--     _ <- parseChar 'e'
--     _ <- parseChar ' '
--     _ <- parseChar 'u'
--     _ <- parseChar 'r'
--     _ <- parseChar 'l'
--     _ <- parseChar '='
--     link <- parseStringQuoted
--     _ <- parseChar '>'
--     bold <- parseBold
--     italic <- parseItalic
--     code <- parseCode
--     return $ ParserImage (link, ParserParagraph []) --------------------------------- idk

-- Parse paragraphs in the body of the Xml file
parseXmlParagraph :: Parser ParserValue
parseXmlParagraph = do
    _ <- skipAll
    _ <- parseStr "<paragraph>"
    _ <- skipAll
    paragraph <- manyTill parseXmlString (parseStr "</paragraph>")
    _ <- skipAll
    return $ ParserParagraph paragraph

-- Parse code block in the body of the Xml file
-- parseCodeBlock :: Parser ParserValue
-- parseCodeBlock = do
--     _ <- skipAll
--     _ <- parseChar '<'
--     _ <- parseChar 'c'
--     _ <- parseChar 'o'
--     _ <- parseChar 'd'
--     _ <- parseChar 'e'
--     _ <- parseChar 'b'
--     _ <- parseChar 'l'
--     _ <- parseChar 'o'
--     _ <- parseChar 'c'
--     _ <- parseChar 'k'
--     _ <- parseChar '>'
--     _ <- skipAll
--     paragraph <- parseXmlParagraph
--     return $ ParserCodeBlock []

-- Parse list in the body of the Xml file
-- parseList :: Parser ParserValue
-- parseList = do
--     _ <- skipAll
--     _ <- parseChar '<'
--     _ <- parseChar 'l'
--     _ <- parseChar 'i'
--     _ <- parseChar 's'
--     _ <- parseChar 't'
--     _ <- parseChar '>'
--     _ <- skipAll
--     paragraph <- parseXmlParagraph
--     return $ ParserArray []

parseXmlBody :: Parser ParserValue
parseXmlBody = do
    _ <- parseStr "<body>"
    _ <- trace "inbody" skipAll
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
    <|> parseXmlParagraph
