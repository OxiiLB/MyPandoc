{--
-- EPITECH PROJECT, 2024
-- Lib
-- File description:
-- Lib
--}

module Lib
    ( parseChar
    , runParser
    , parseAnyChar
    -- , parseOr
    -- , parseAnd
    -- , parseAndWith
    -- , parseMany
    -- , parseSome
    -- , parseUInt
    -- , parseInt
    -- , parseTuple
    ) where

data Parser a = Parser {
    runParser :: String -> Maybe (a , String )
}

parseChar :: Char -> Parser Char
parseChar c = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> if x == c
              then Just (x, xs)
              else Nothing

parseAnyChar :: String -> Parser Char
parseAnyChar cs = Parser $ \s -> case s of
    [] -> Nothing
    (x:xs) -> if x `elem` cs
              then Just (x, xs)
              else Nothing

-- parseAnyChar :: String -> Parser Char
-- parseAnyChar _ [] = Nothing
-- parseAnyChar [] _ = Nothing
-- parseAnyChar (c:cs) (x:xs) = if x == c
--                              then Just (x, xs)
--                              else parseAnyChar cs (x:xs)

-- parseOr :: Parser a -> Parser a -> Parser a
-- parseOr f1 f2 s = case f1 s of
--                     Nothing -> f2 s
--                     Just (a, rest) -> Just (a, rest)

-- parseAnd :: Parser a -> Parser b -> Parser (a, b)
-- parseAnd f1 f2 s = case f1 s of
--                     Nothing -> Nothing
--                     Just (a, rest) -> case f2 rest of
--                         Nothing -> Nothing
--                         Just (b, c) -> Just ((a, b), c)

-- parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
-- parseAndWith f f1 f2 s = case f1 s of
--                     Nothing -> Nothing
--                     Just (a, rest) -> case f2 rest of
--                         Nothing -> Nothing
--                         Just (b, c) -> Just (f a b, c)

-- parseMany :: Parser a -> Parser [a]
-- parseMany f s = case f s of
--                     Nothing -> Just ([], s)
--                     Just (a, rest) -> case parseMany f rest of
--                         Nothing -> Nothing
--                         Just (as, c) -> Just (a:as, c)

-- parseSome :: Parser a -> Parser [a]
-- parseSome f s = case f s of
--                     Nothing -> Nothing
--                     Just (a, rest) -> case parseMany f rest of
--                         Nothing -> Just ([a], rest)
--                         Just (as, c) -> Just (a:as, c)

-- parseUInt :: Parser Int
-- parseUInt s = case parseSome (parseAnyChar ['0'..'9']) s of
--                 Nothing -> Nothing
--                 Just (as, rest) -> Just (read as, rest)

-- parseInt :: Parser Int
-- parseInt s = case parseOr (parseAndWith (\_ b -> b) (parseChar '-') parseUInt)
--                 parseUInt s of
--                 Nothing -> Nothing
--                 Just (a, rest) -> Just (a, rest)

-- parseTuple :: Parser a -> Parser (a, a)
-- parseTuple f s = case parseChar '(' s of
--                     Nothing -> Nothing
--                     Just (_,y) -> case parseAndWith (\a (_, b) -> (a, b)) f
--                         (parseAnd (parseChar ',') f) y of
--                         Nothing -> Nothing
--                         Just (a, res) -> case parseChar ')' res of
--                             Nothing -> Nothing
--                             Just (_, rest) -> Just (a, rest)
