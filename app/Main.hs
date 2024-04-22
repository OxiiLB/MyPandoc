{--
-- EPITECH PROJECT, 2024
-- main
-- File description:
-- Main function of the project
--}

module Main (main) where

import Lib

main :: IO ()
main = print $ runParser (parseChar 'a') "abc"
