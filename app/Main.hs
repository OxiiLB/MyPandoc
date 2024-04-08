module Main (main) where

import Lib

main :: IO ()
main = print $ parseChar 'a' "aaaa"

