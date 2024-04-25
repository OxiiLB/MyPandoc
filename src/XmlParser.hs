{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- XmlParser
--}

module XmlParser(
    parseXmlValue
)where

import Parser

parseXmlValue :: Parser XmlValue
parseXmlValue = parseXmlString <|> parseXmlArray <|> parseXmlObject