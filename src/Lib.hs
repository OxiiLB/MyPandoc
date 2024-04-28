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
import XmlParser
-- import OutputMarkdown ( writeMarkdownFile )
import OutputXml (writeXmlFile)
import OutputJson
import JsonParser
import Data.Maybe(isNothing)
import Data.List (isSuffixOf)
import System.Exit (exitWith, ExitCode(ExitFailure), exitSuccess)
import Control.Applicative (Alternative(..))

data Format = JSON | XML | Markdown deriving (Show, Eq)

data Info = Info {filePath :: Maybe String, inputFormat :: Maybe Format, outputFormat :: Maybe Format, outputFile :: Maybe String} deriving Show

parseAllType :: Parser ParserValue
parseAllType = parseJsonValue  <|> parseXmlValue

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
sendToParser file info format =
    case format of
        JSON -> case runParser parseAllType file of
            Just (parsedJson, _) -> writeJsonFile (outputFile info) parsedJson
                >> exitSuccess
            Nothing -> putStrLn "Error: Invalid JSON file"
                >> exitWith (ExitFailure 84)
        XML -> case runParser parseAllType file of
            Just (parsedXml, _) -> writeXmlFile (outputFile info) parsedXml
            Nothing -> putStrLn "Error: Invalid XML file"
                >> exitWith (ExitFailure 84)
        -- Markdown -> case runParser parseAllType file of
        --     Just (parsedMarkdown, remaining) -> exitSuccess
        --         -- writeMarkdownFile (outputFile info) parsedMarkdown
        --     Nothing -> putStrLn "Error: Invalid Markdown file"
        --         >> exitWith (ExitFailure 84)
        _ -> exitWith (ExitFailure 84)

parseFile :: Maybe String -> Info -> IO ()
parseFile Nothing _ = exitWith (ExitFailure 84)
parseFile (Just file) info | isNothing (inputFormat info) =
    parseFile (Just file) (info { inputFormat = detectFormat (filePath info) })
parseFile (Just file) info = -- will send file to functions, but for now, im just using exitSuccess as a placeholder
  case outputFormat info of
    Nothing -> exitWith (ExitFailure 84)
    _ -> sendToParser file info (fromJust $ outputFormat info)
    where fromJust (Just a) = a
          fromJust Nothing = error "fromJust: Nothing"
