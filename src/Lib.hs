{--
-- EPITECH PROJECT, 2024
-- Lib
-- File description:
-- Lib
--}

module Lib
    (
        Format(..),
        Info(..),
        defaultInfo,
        parseFile
    ) where

import Parser
import JsonParser ( parseJsonValue )
import XmlParser ()
import OutputJson ( writeJsonFile )
import OutputMarkdown ( writeMarkdownFile )
import OutputXml (writeXmlFile)
import Data.Maybe(isNothing)
import Data.List (isSuffixOf)
import System.Exit (exitWith, ExitCode(ExitFailure))

data Format = JSON | XML | Markdown deriving (Show, Eq)

data Info = Info {filePath :: Maybe String, inputFormat :: Maybe Format, outputFormat :: Maybe Format, outputFile :: Maybe String} deriving Show

parseAllType :: Parser ParserValue
parseAllType = parseJsonValue -- <|> parseXmlValue <|> parseMarkdownValue

defaultInfo :: Info
defaultInfo = Info {filePath = Nothing, inputFormat = Nothing,
                outputFormat = Nothing, outputFile = Nothing}

detectFormat :: Maybe String -> Maybe Format
detectFormat Nothing = Nothing
detectFormat (Just file) 
    | ".json" `isSuffixOf` file = Just JSON
    | ".xml" `isSuffixOf` file = Just XML
    | ".md" `isSuffixOf` file || ".markdown" `isSuffixOf` file = Just Markdown
    | otherwise = Nothing

sendToParser :: String -> Info -> Format -> IO ()
sendToParser file info format = case runParser parseAllType file of
    Just (parsedData, _) -> case format of
        JSON -> writeJsonFile (outputFile info) parsedData
        XML -> writeXmlFile (outputFile info) parsedData
        Markdown -> writeMarkdownFile (outputFile info) parsedData
    Nothing -> exitWith (ExitFailure 84)

parseFile :: Maybe String -> Info -> IO ()
parseFile Nothing _ = exitWith (ExitFailure 84)
parseFile (Just file) info | isNothing (inputFormat info) =
    parseFile (Just file) (info { inputFormat = detectFormat (filePath info) })
parseFile (Just file) info =
    case outputFormat info of
        Nothing -> exitWith (ExitFailure 84)
        Just format -> sendToParser file info format
