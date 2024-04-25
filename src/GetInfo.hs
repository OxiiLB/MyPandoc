{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- GetInfo
--}

module GetInfo
    (
        getInputFile,
        getInfoArgs,
    ) where

import System.Environment()
import System.IO()
import Control.Exception
import Data.Maybe ()
import Lib (Format(..), Info(..))

stringToFormat :: String -> Maybe Format
stringToFormat "json" = Just JSON
stringToFormat "xml" = Just XML
stringToFormat "markdown" = Just Markdown
stringToFormat _ = Nothing

getInfoArgs :: [String] -> Info -> Info
getInfoArgs [] info = info
getInfoArgs ("-i":path:xs) info =
    getInfoArgs xs (info { filePath = Just path })
getInfoArgs ("-f":format:xs) info =
    getInfoArgs xs (info { outputFormat = stringToFormat format })
getInfoArgs ("-e":format:xs) info =
    getInfoArgs xs (info { inputFormat = stringToFormat format })
getInfoArgs ("-o":file:xs) info =
    getInfoArgs xs (info { outputFile = Just file })
getInfoArgs (_:xs) info = getInfoArgs xs info

getInputFile :: [String] -> IO (Maybe String)
getInputFile [] = return Nothing
getInputFile ("-i":file:_) = do
    result <- try $ readFile file
    case result of
        Left ex -> putStrLn ("Error reading file: " ++ show (ex :: IOError))
            >> return Nothing
        Right contents -> return (Just contents)
getInputFile (_:xs) = getInputFile xs
