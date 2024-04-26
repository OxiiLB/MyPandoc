{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- XmlParser
--}

module XmlParser
    (
    ) where

import Control.Applicative (Alternative(..))
import Parser
    ( ParserValue(ParserHead, ParserArray, ParserString, ParserObject),
      Parser(Parser),
      parseChar,
      parseAnyChar )
import Control.Monad (void)
import Data.List ()

-- Parse XML string value
parseStringQuoted :: Parser String
parseStringQuoted = parseChar '"' *> many (parseAnyChar
            (['a' .. 'z'] ++ ['A' .. 'Z'] ++ " " ++ ['0' .. '9'] ++ "-"
               ++ "." ++ "_" ++ "/" ++ "\\" ++ ":" ++ "@" ++ "*" ++ "&"
               ++ "%" ++ "+" ++ "=" ++ "!" ++ "?" ++ "#" ++ "$" ++ "^"
               ++ "(" ++ ")" ++ "[" ++ "]" ++ "{" ++ "}" ++ "<" ++ ">"
               ++ "," ++ ";" ++ "'" ++ "`" ++ "~" ++ "|" ++ " "))
              <* parseChar '"'

parseString :: Parser String
parseString = many (parseAnyChar
            (['a' .. 'z'] ++ ['A' .. 'Z'] ++ " " ++ ['0' .. '9'] ++ "-"
               ++ "." ++ "_" ++ "/" ++ "\\" ++ ":" ++ "@" ++ "*" ++ "&"
               ++ "%" ++ "+" ++ "=" ++ "!" ++ "?" ++ "#" ++ "$" ++ "^"
               ++ "(" ++ ")" ++ "[" ++ "]" ++ "{" ++ "}" ++ "<" ++ ">"
               ++ "," ++ ";" ++ "'" ++ "`" ++ "~" ++ "|" ++ " "))

-- createXmlArray :: Parser [ParserValue]
-- createXmlArray = checkBegin "<list>" *>
--     skipAll *> parseListSeparated parseXmlValue <* skipAll
--     <* checkBegin "</list>"

-- -- Parse Xml array value
-- parseXmlArray :: Parser ParserValue
-- parseXmlArray = ParserArray <$> createXmlArray

-- -- Parse Xml string value
-- parseXmlString :: Parser ParserValue
-- parseXmlString = ParserString <$> parseString <* skipAll

-- -- -- Parse Xml section value
-- -- parseXmlSection :: Parser ParserValue
-- -- parseXmlSection = ParserObject <$> (checkBegin "<section title=" *>
-- --     parseChar '\"' *> skipAll *> 
-- --     <* skipAll
-- --     <* checkBegin "</section>")

-- Parse list separated values
-- parseListSeparated :: Parser a -> Parser [a]
-- parseListSeparated p =
--     (:) <$> p <*> many (skipAll *> checkBegin "</" *> skipAll  *> parseChar '>' *> skipAll *> p) <|> pure []

-- -- Parse Xml body value
-- parseXmlBody :: Parser ParserValue
-- parseXmlBody = ParserObject <$> (checkBegin "<body>" *> skipAll *>
--     parseListSeparated ((,) <$> parseString <* skipAll <* parseXmlValue) <* skipAll
--     <* checkBegin "</body>")

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
    _ <- skipAll
    date <- parseStringQuoted
    _ <- skipAll
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
    _ <- skipAll
    author <- parseStringQuoted
    _ <- skipAll
    _ <- parseChar '<'
    _ <- parseChar '/'
    _ <- parseChar 'a'
    _ <- parseChar 'u'
    _ <- parseChar 't'
    _ <- parseChar 'h'
    _ <- parseChar 'o'
    _ <- parseChar 'r'
    _ <- parseChar '>'
    _ <- skipAll
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
    return $ ParserHead title Nothing Nothing

parseXmlBody :: Parser ParserValue
parseXmlBody = do
    _ <- skipDocument
    _ <- skipAll
    _ <- '<'
    _ <- 'b'
    _ <- 'o'
    _ <- 'd'
    _ <- 'y'
    _ <- '>'
    

skipAll :: Parser ()
skipAll = void $ many $ parseAnyChar " \n\r\t"

-- Complete Xml value parser
parseXmlValue :: Parser ParserValue
parseXmlValue = skipAll *> parseXmlHeader