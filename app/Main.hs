{--
-- EPITECH PROJECT, 2024
-- main
-- File description:
-- Main function of the project
--}

module Main (main) where

import ErrorHandling (checkArgs)
import Parser
import Lib
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure), exitSuccess)

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
        True -> exitSuccess -------------------------------- place holder
