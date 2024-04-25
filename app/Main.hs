{--
-- EPITECH PROJECT, 2024
-- main
-- File description:
-- Main function of the project
--}

module Main (main) where

import ErrorHandling (checkArgs)
import GetInfo(getInputFile, getInfoArgs)
import Lib(parseFile, defaultInfo)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))

usage :: IO ()
usage = putStrLn "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]\
                \\n\n\tifile\tpath to the file to convert\n\toformat\toutput \
                \format (xml, json, markdown)\n\tofile\tpath to the output \
                \file\n\tiformat\tinput format (xml, json, markdown)"

main :: IO ()
main = do
    args <- getArgs
    case checkArgs args of
        False -> usage >> exitWith (ExitFailure 84)
        True -> do
            maybeFile <- getInputFile args
            case maybeFile of
                Nothing -> exitWith (ExitFailure 84)
                Just file -> parseFile (Just file)
                    (getInfoArgs args defaultInfo)