{--
-- EPITECH PROJECT, 2024
-- main
-- File description:
-- Main function of the project
--}

module Main (main) where

import Lib

usage :: IO ()
usage = putStrLn "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]\
                \\n\t ifile\tpath to the file to convert\n\toformat\toutput \
                \format (xml, json, markdown)\n\tofile\tpath to the output \
                \file\n\tiformat\tinput format (xml, json, markdown)"

main :: IO ()
main = print $ parseChar 'a' "aaaa"
