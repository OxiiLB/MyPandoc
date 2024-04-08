{--
-- EPITECH PROJECT, 2024
-- main
-- File description:
-- Main function of the project
--}

module Main (main) where

import Lib

main :: IO ()
main = print $ parseAndWith  (\ x y -> [x , y ]) (parseChar 'a') (parseChar 'b') "abcd"

