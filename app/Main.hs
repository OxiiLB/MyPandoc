{--
-- EPITECH PROJECT, 2024
-- main
-- File description:
-- Main function of the project
--}

module Main (main) where

import ErrorHandling
import Parser
import Lib

usage :: IO ()
usage = putStrLn "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]\
                \\n\n\tifile\tpath to the file to convert\n\toformat\toutput \
                \format (xml, json, markdown)\n\tofile\tpath to the output \
                \file\n\tiformat\tinput format (xml, json, markdown)"

main :: IO ()
main = usage
