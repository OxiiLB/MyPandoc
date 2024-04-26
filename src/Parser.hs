{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- Parser
--}

module Parser
    ( parseChar
    , parseAnyChar
    , parseOr
    , parseAnd
    , parseAndWith
    , parseMany
    , parseSome
    , parseUInt
    , parseInt
    , parseTuple
    , parseStringQuoted
    , Parser(..)
    , ParserValue(..)
    ) where
import Control.Applicative (Alternative(..))
import Control.Monad (ap)

newtype Parser a = Parser {
    runParser :: String -> Maybe (a , String )
}

instance Monad Parser where
    return = pure
    p >>= f = Parser $ \s -> case runParser p s of
        Nothing -> Nothing
        Just (a, rest) -> runParser (f a) rest

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    (<*>) = ap

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2 = Parser $ \s -> runParser (parseOr p1 p2) s
  many p = Parser $ \s -> runParser (parseMany p) s
  some p = Parser $ \s -> runParser (parseSome p) s

instance Functor Parser where
    fmap fct parser = Parser $ \s -> case runParser parser s of
        Nothing -> Nothing
        Just (a, rest) -> Just (fct a, rest)

data ParserValue
    = ParserString String
    | ParserItalic String
    | ParserBold String
    | ParserCode String
    | ParserLink (String, ParserValue)
    | ParserImage (String, ParserValue)
    | ParserList [ParserValue]
    | ParserCodeBlock [ParserValue]
    | ParserSection String [ParserValue]
    | ParserParagraphe [ParserValue]
    | ParserHead String (Maybe String) (Maybe String)
    | ParserBody [ParserValue]
    | ParserArray [ParserValue]
    | ParserObject [ParserValue]
    deriving (Show)

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

parseAndWith :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
parseAndWith f f1 f2 = f <$> f1 <*> f2

parseMany :: Parser a -> Parser [a]
parseMany f = someOrNone
    where
        someOrNone = (:) <$> f <*> parseMany f <|> pure []

parseSome :: Parser a -> Parser [a]
parseSome f = (:) <$> f <*> parseMany f

parseInt :: Parser Int
parseInt = read <$> parseSome (parseAnyChar ['0'..'9'])

parseUInt :: Parser Int
parseUInt = Parser $ \s -> 
    case runParser (parseSome (parseAnyChar ['0'..'9'])) s of
        Nothing -> Nothing
        Just (as, rest) -> Just (read as, rest)

parseTuple :: Parser a -> Parser (a, a)
parseTuple f = (,) <$> f <* parseChar ',' <*> f

parseStringQuoted :: Parser String
parseStringQuoted = parseChar '\"' *> many (parseAnyChar
            (['a' .. 'z'] ++ ['A' .. 'Z'] ++ " " ++ ['0' .. '9'] ++ "-"
               ++ "." ++ "_" ++ "/" ++ "\\" ++ ":" ++ "@" ++ "*" ++ "&"
               ++ "%" ++ "+" ++ "=" ++ "!" ++ "?" ++ "#" ++ "$" ++ "^"
               ++ "(" ++ ")" ++ "[" ++ "]" ++ "{" ++ "}" ++ "<" ++ ">"
               ++ "," ++ ";" ++ "'" ++ "`" ++ "~" ++ "|" ++ " "))
              <* parseChar '\"'