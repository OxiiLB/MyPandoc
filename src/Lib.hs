{--
-- EPITECH PROJECT, 2024
-- Lib
-- File description:
-- Lib
--}

module Lib
    ( parseChar
    , parseAnyChar
    , parseOr
    , parseAnd
    , parseAndWith
    , parseMany
    , parseSome
    ) where

type Parser a = String -> Maybe (a , String )

parseChar :: Char -> Parser Char
parseChar _ [] = Nothing
parseChar c (x:xs) = if x == c
                   then Just (x, xs)
                   else Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar _ [] = Nothing
parseAnyChar [] _ = Nothing
parseAnyChar (c:cs) (x:xs) = if x == c
                             then Just (x, xs)
                             else parseAnyChar cs (x:xs)

parseOr :: Parser a -> Parser a -> Parser a
parseOr f1 f2 s = case f1 s of
                    Nothing -> f2 s
                    Just (a, s') -> Just (a, s')

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd f1 f2 s = case f1 s of
                    Nothing -> Nothing
                    Just (a, s') -> case f2 s' of
                        Nothing -> Nothing
                        Just (b, s'') -> Just ((a, b), s'')

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f f1 f2 s = case f1 s of
                    Nothing -> Nothing
                    Just (a, s') -> case f2 s' of
                        Nothing -> Nothing
                        Just (b, s'') -> Just (f a b, s'')

parseMany :: Parser a -> Parser [a]
parseMany f s = case f s of
                    Nothing -> Just ([], s)
                    Just (a, s') -> case parseMany f s' of
                        Nothing -> Nothing
                        Just (as, s'') -> Just (a:as, s'')

parseSome :: Parser a -> Parser [a]
parseSome f s = case f s of
                    Nothing -> Nothing
                    Just (a, s') -> case parseMany f s' of
                        Nothing -> Just ([a], s')
                        Just (as, s'') -> Just (a:as, s'')
