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
import Control.Monad (void)
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
    _ <- parseChar '<'
    _ <- parseChar 'd'
    _ <- parseChar 'a'
    _ <- parseChar 't'
    _ <- parseChar 'e'
    _ <- parseChar '>'
    date <- parseString
    _ <- parseChar '<'
    _ <- parseChar '/'
    _ <- parseChar 'd'
    _ <- parseChar 'a'
    _ <- parseChar 't'
    _ <- parseChar 'e'
    _ <- parseChar '>'
    _ <- skipAll
    return $ Just date

-- Parse author in header of the Xml file
parseAuthor :: Parser (Maybe String)
parseAuthor = do
    _ <- skipAll
    _ <- parseChar '<'
    _ <- parseChar 'a'
    _ <- parseChar 'u'
    _ <- parseChar 't'
    _ <- parseChar 'h'
    _ <- parseChar 'o'
    _ <- parseChar 'r'
    _ <- parseChar '>'
    author <- parseString
    _ <- parseChar '<'
    _ <- parseChar '/'
    _ <- parseChar 'a'
    _ <- parseChar 'u'
    _ <- parseChar 't'
    _ <- parseChar 'h'
    _ <- parseChar 'o'
    _ <- parseChar 'r'
    _ <- parseChar '>'
    return $ Just author

-- skip document
skipDocument :: Parser ()
skipDocument = do
    _ <- skipAll
    _ <- parseChar '<'
    _ <- parseChar 'd'
    _ <- parseChar 'o'
    _ <- parseChar 'c'
    _ <- parseChar 'u'
    _ <- parseChar 'm'
    _ <- parseChar 'e'
    _ <- parseChar 'n'
    _ <- parseChar 't'
    _ <- parseChar '>'
    _ <- skipAll
    return ()

-- Parse header of the Xml file
parseXmlHeader :: Parser ParserValue
parseXmlHeader = do
    _ <- skipDocument
    _ <- skipAll
    _ <- parseChar '<'
    _ <- parseChar 'h'
    _ <- parseChar 'e'
    _ <- parseChar 'a'
    _ <- parseChar 'd'
    _ <- parseChar 'e'
    _ <- parseChar 'r'
    _ <- parseChar ' '
    _ <- parseChar 't'
    _ <- parseChar 'i'
    _ <- parseChar 't'
    _ <- parseChar 'l'
    _ <- parseChar 'e'
    _ <- parseChar '='
    title <- parseStringQuoted
    _ <- parseChar '>'
    author <- parseAuthor
    date <- parseDate
    return $ ParserHead title author date

-- Parse code in the body of the Xml file
parseCode :: Parser ParserValue
parseCode = do
    _ <- skipAll
    _ <- parseChar '<'
    _ <- parseChar 'c'
    _ <- parseChar 'o'
    _ <- parseChar 'd'
    _ <- parseChar 'e'
    _ <- parseChar '>'
    _ <- skipAll
    code <- parseString
    _ <- skipAll
    _ <- parseChar '<'
    _ <- parseChar '/'
    _ <- parseChar 'c'
    _ <- parseChar 'o'
    _ <- parseChar 'd'
    _ <- parseChar 'e'
    _ <- parseChar '>'
    _ <- skipAll
    return $ ParserCode code

-- Parse italic in the body of the Xml file
parseItalic :: Parser ParserValue
parseItalic = do
    _ <- skipAll
    _ <- parseChar '<'
    _ <- parseChar 'i'
    _ <- parseChar 't'
    _ <- parseChar 'a'
    _ <- parseChar 'l'
    _ <- parseChar 'i'
    _ <- parseChar 'c'
    _ <- parseChar '>'
    _ <- skipAll
    italic <- parseString
    _ <- skipAll
    _ <- parseChar '<'
    _ <- parseChar '/'
    _ <- parseChar 'i'
    _ <- parseChar 't'
    _ <- parseChar 'a'
    _ <- parseChar 'l'
    _ <- parseChar 'i'
    _ <- parseChar 'c'
    _ <- parseChar '>'
    _ <- skipAll
    return $ ParserItalic italic

-- Parse bold in the body of the Xml file
parseBold :: Parser ParserValue
parseBold = do
    _ <- skipAll
    _ <- parseChar '<'
    _ <- parseChar 'b'
    _ <- parseChar 'o'
    _ <- parseChar 'l'
    _ <- parseChar 'd'
    _ <- parseChar '>'
    _ <- skipAll
    bold <- parseString
    _ <- skipAll
    _ <- parseChar '<'
    _ <- parseChar '/'
    _ <- parseChar 'b'
    _ <- parseChar 'o'
    _ <- parseChar 'l'
    _ <- parseChar 'd'
    _ <- parseChar '>'
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
-- parseParagraph :: Parser ParserValue
-- parseParagraph = do
--     _ <- skipAll
--     _ <- parseChar '<'
--     _ <- parseChar 'p'
--     _ <- parseChar 'a'
--     _ <- parseChar 'r'
--     _ <- parseChar 'a'
--     _ <- parseChar 'g'
--     _ <- parseChar 'r'
--     _ <- parseChar 'a'
--     _ <- parseChar 'p'
--     _ <- parseChar 'h'
--     _ <- parseChar '>'
--     bold <- parseBold
--     italic <- parseItalic
--     code <- parseCode
--     link <- parseLink
--     image <- parseImage
--     return $ ParserParagraph []

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
--     paragraph <- parseParagraph
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
--     paragraph <- parseParagraph
--     return $ ParserArray []

-- Parse body of the Xml file
-- parseXmlBody :: Parser ParserValue
-- parseXmlBody = do
--     _ <- skipAll
--     _ <- parseChar '<'
--     _ <- parseChar 'b'
--     _ <- parseChar 'o'
--     _ <- parseChar 'd'
--     _ <- parseChar 'y'
--     _ <- parseChar '>'
--     section <- parseSection
--     paragraph <- parseParagraph
--     return $ ParserBody []

parserXmlBody :: Parser ParserValue
parserXmlBody = do
    _ <- skipAll
    _ <- parseChar '<'
    _ <- parseChar 'b'
    _ <- parseChar 'o'
    _ <- parseChar 'd'
    _ <- parseChar 'y'
    _ <- parseChar '>'
    _ <- skipAll
    body <- parseCommaSeparated parseXmlValue
    _ <- skipAll
    _ <- parseChar '<'
    _ <- parseChar '/'
    _ <- parseChar 'b'
    _ <- parseChar 'o'
    _ <- parseChar 'd'
    _ <- parseChar 'y'
    _ <- parseChar '>'
    return $ ParserBody body

-- Complete Xml value parser
parseXmlValue :: Parser ParserValue
parseXmlValue = skipAll *> parseXmlHeader <|> parserXmlBody <|> parseCode <|>
    parseItalic <|> parseBold -- <|>  parseLink <|> parseImage <|>
    -- parseParagraph <|> parseCodeBlock <|> parseList
