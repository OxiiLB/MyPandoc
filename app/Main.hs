{--
-- EPITECH PROJECT, 2024
-- main
-- File description:
-- Main function of the project
--}

module Main (main) where

import Lib

main :: IO ()
main = print $ parseSome (parseAnyChar ['0'..'9']) "foobar42"

