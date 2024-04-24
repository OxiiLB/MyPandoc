--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Parsing
--

module Parsing
    (
        parseXmlFile
    ) where

-- [("header", Header), ("body", Body)]

-- Header {title :: String, author ::Maybe String, date :: Maybe String}

-- Body {structural :: Structural, formatting :: Formatting, link :: String, image :: String, list :: [String]}

-- Structural {text :: String, paragraph :: String, section :: String, codeBlock :: String}
-- Formatting {bold :: String, italic :: String, code :: String}

-- parseXmlFile :: String -> [(String, Object)]
