{--
-- EPITECH PROJECT, 2024
-- Lib
-- File description:
-- Lib
--}

module Lib
    ( parseChar
    ) where

type Parser a = String -> Maybe (a , String )

parseChar :: Char -> Parser Char
parseChar _ [] = Nothing
parseChar c (x:xs) = if x == c
                   then Just (x, xs)
                   else Nothing