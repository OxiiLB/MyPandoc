{--
-- EPITECH PROJECT, 2024
-- main
-- File description:
-- Main function of the project
--}

module Main (main) where

import Lib

main :: IO ()
main = print $ parseTuple parseInt "(123,456)foo bar"

