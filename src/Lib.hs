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
import OutputMarkdown ( writeMarkdownFile )
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

xmlConverter :: String -> Info -> IO ()
xmlConverter file info = case runParser parseAllType file of
    Just (parsedXml, _) -> writeXmlFile (outputFile info) parsedXml
        >> exitSuccess
    Nothing -> putStrLn "Error: Invalid file"
        >> exitWith (ExitFailure 84)

jsonConverter :: String -> Info -> IO ()
jsonConverter file info = case runParser parseAllType file of
    Just (parsedJson, _) -> writeJsonFile (outputFile info) parsedJson
        >> exitSuccess
    Nothing -> putStrLn "Error: Invalid file"
        >> exitWith (ExitFailure 84)

markdownConverter :: String -> Info -> IO ()
markdownConverter file info = case runParser parseAllType file of
    Just (parsedMarkdown, _) ->
        writeMarkdownFile (outputFile info) parsedMarkdown
        >> exitSuccess
    Nothing -> putStrLn "Error: Invalid file"
        >> exitWith (ExitFailure 84)

sendToParser :: String -> Info -> Format -> IO ()
sendToParser file info format =
    case format of
        JSON -> jsonConverter file info
        XML -> xmlConverter file info
        Markdown -> markdownConverter file info

sendParser :: String -> Info -> IO ()
sendParser file info = case outputFormat info of
                Nothing -> putStrLn "type ./mypandoc -help"
                    >> exitWith (ExitFailure 84)
                _ -> sendToParser file info (fromJust $ outputFormat info)
  where fromJust (Just a) = a
        fromJust Nothing = error "error: Nothing"

parseFile :: Maybe String -> Info -> IO ()
parseFile Nothing _ = exitWith (ExitFailure 84)
parseFile (Just file) info | isNothing (inputFormat info) =
    parseFile (Just file) (info { inputFormat = detectFormat (filePath info) })
    | otherwise = let detectedFormat = detectFormat (filePath info)
        in if detectedFormat /= inputFormat info
            then putStrLn "Given inputFormat doesn't match actual inputFormat"
                >> exitWith (ExitFailure 84)
            else sendParser file info
