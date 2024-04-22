{--
-- EPITECH PROJECT, 2024
-- Lib
-- File description:
-- Lib
--}
{-# LANGUAGE InstanceSigs #-}

module Lib
    ( parseChar
    , runParser
    , parseAnyChar
    , parseOr
    , parseAnd
    , parseAndWith
    , parseMany
    , parseSome
    , parseUInt
    , parseInt
    , parseTuple
    ) where
import Control.Applicative()
import Control.Applicative (Alternative(..))
import Control.Monad (ap)


data Parser a = Parser {
    runParser :: String -> Maybe (a , String )
}

instance Monad Parser where
    return = pure
    p >>= f = Parser $ \s -> case runParser p s of
        Nothing -> Nothing
        Just (a, rest) -> runParser (f a) rest

-- Define `ap` for Parser to avoid orphan instances
instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    (<*>) = ap



instance Alternative Parser where
    empty = Parser $ const Nothing
    p1 <|> p2 = Parser $ \s -> case runParser p1 s of
        Nothing -> runParser p2 s
        Just (a, rest) -> Just (a, rest)


-- instance Applicative Parser where
--     pure x = Parser $ \s -> Just (x, s)
--     pf <*> px = Parser $ \s -> case runParser pf s of
--         Nothing -> Nothing
--         Just (f, rest) -> case runParser px rest of
--             Nothing -> Nothing
--             Just (x, rest') -> Just (f x, rest')


instance Functor Parser where
    fmap fct parser = Parser $ \s -> case runParser parser s of
        Nothing -> Nothing
        Just (a, rest) -> Just (fct a, rest)

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

parseOr :: Parser a -> Parser a -> Parser a
parseOr f1 f2 = Parser $ \s -> case runParser f1 s of
    Nothing -> runParser f2 s
    Just (a, rest) -> Just (a, rest)

parseAnd :: Parser a -> Parser b -> Parser (a, b)
parseAnd f1 f2 = Parser $ \s -> case runParser f1 s of
    Nothing -> Nothing
    Just (a, rest) -> case runParser f2 rest of
        Nothing -> Nothing
        Just (b, c) -> Just ((a, b), c)

-- STEP 2.3
parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f f1 f2 = f <$> f1 <*> f2

-- parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
-- parseAndWith f f1 f2 = Parser $ \s -> case runParser f1 s of
--     Nothing -> Nothing
--     Just (a, rest) -> case runParser f2 rest of
--         Nothing -> Nothing
--         Just (b, c) -> Just (f a b, c)

-- STEP 2.4
parseMany :: Parser a -> Parser [a]
parseMany f = someOrNone
    where
        someOrNone = (:) <$> f <*> parseMany f <|> pure []

-- parseMany :: Parser a -> Parser [a]
-- parseMany f = Parser $ \s -> case runParser f s of
--     Nothing -> Just ([], s)
--     Just (a, rest) -> case runParser (parseMany f) rest of
--         Nothing -> Nothing
--         Just (as, c) -> Just (a:as, c)

-- STEP 2.3
parseSome :: Parser a -> Parser [a]
parseSome f = (:) <$> f <*> parseMany f

-- parseSome :: Parser a -> Parser [a]
-- parseSome f = Parser $ \s -> case runParser f s of
--     Nothing -> Nothing
--     Just (a, rest) -> case runParser (parseMany f) rest of
--         Nothing -> Just ([a], rest)
--         Just (as, c) -> Just (a:as, c)

-- STEP 2.2 and 2.3 and 2.4 <$>
parseInt :: Parser Int
parseInt = read <$> parseSome (parseAnyChar ['0'..'9'])

-- STEP 2.2 FMAP
-- parseInt :: Parser Int
-- parseInt = fmap read (parseSome (parseAnyChar ['0'..'9']))

-- parseInt :: Parser Int
-- parseInt = Parser $ \s -> case runParser (parseSome (parseAnyChar ['0'..'9'])) s of
--     Nothing -> Nothing
--     Just (as, rest) -> Just (read as, rest)

parseUInt :: Parser Int
parseUInt = Parser $ \s -> case runParser (parseSome (parseAnyChar ['0'..'9'])) s of
    Nothing -> Nothing
    Just (as, rest) -> Just (read as, rest)

-- STEP 2.5
parseTuple :: Parser (Int, Int, Int)
parseTuple = do
    parseChar '('
    a <- parseInt
    parseChar ','
    b <- parseInt
    parseChar ','
    c <- parseInt
    parseChar ')'
    return (a, b, c)


-- STEP 2.3
-- parseTuple :: Parser a -> Parser (a, a)
-- parseTuple f = (,) <$> f <* parseChar ',' <*> f

-- parseTuple :: Parser a -> Parser (a, a)
-- parseTuple f s = case parseChar '(' s of
--                     Nothing -> Nothing
--                     Just (_,y) -> case parseAndWith (\a (_, b) -> (a, b)) f
--                         (parseAnd (parseChar ',') f) y of
--                         Nothing -> Nothing
--                         Just (a, res) -> case parseChar ')' res of
--                             Nothing -> Nothing
--                             Just (_, rest) -> Just (a, rest)
