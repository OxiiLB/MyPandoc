module Lib
    (
    ) where

import Control.Applicative ((<|>), some, many)
import Text.ParserCombinators.Parsec.Char (digit, char, string, oneOf, noneOf)
import Data.Char (chr)
import Numeric (readHex)

-- Define your Parser type and itds associated functions here
data Parser a = Parser {
    runParser :: String -> Maybe (a , String )
}

-- Define your JsonValue data type
data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonNumber Double
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show)

-- Define your parsers for JsonValue
parseJsonValue :: Parser JsonValue
parseJsonValue = parseNull <|> parseBool <|> parseNumber <|> parseString

parseNull :: Parser JsonValue
parseNull = JsonNull <$ string "null"

parseBool :: Parser JsonValue
parseBool = (JsonBool True <$ string "true") <|> (JsonBool False <$ string "false")

parseNumber :: Parser JsonValue
parseNumber = JsonNumber <$> parseFloat

parseString :: Parser JsonValue
parseString = JsonString <$> parseQuotedString

-- parseFloat parser
parseFloat :: Parser Double
parseFloat = read <$> (plus <|> minus <|> num <|> decimal <|> exponent)
  where
    plus = char '+' *> num
    minus = (:) <$> char '-' <*> num
    num = some digit
    decimal = (++) <$> num <*> ((:) <$> char '.' <*> num)
    exponent = do
        e <- oneOf "eE"
        sign <- optional (char '+' <|> char '-')
        exp <- num
        let multiplier = case sign of
                Just '+' -> 1
                _        -> -1
        return $ "e" ++ [e] ++ show (multiplier * exp)

-- parseQuotedString parser
parseQuotedString :: Parser String
parseQuotedString = char '"' *> many (escapedChar <|> noneOf "\"") <* char '"'
  where
    escapedChar = char '\\' *> choice
        [ '"'  <$ char '"'
        , '\\' <$ char '\\'
        , '/'  <$ char '/'
        , '\b' <$ char 'b'
        , '\f' <$ char 'f'
        , '\n' <$ char 'n'
        , '\r' <$ char 'r'
        , '\t' <$ char 't'
        , '\v' <$ char 'v'
        , '&' <$ string "&amp;" -- XML escape for "&"
        , '\t' <$ string "&gt;"  -- XML escape for ">"
        , '\t' <$ string "&lt;"  -- XML escape for "<"
        , '\t' <$ string "&quot;"-- XML escape for """
        , '\t' <$ string "&apos;"-- XML escape for "'"
        , hexChar
        ]
    hexChar = char 'u' *> (chr . fromInteger . fst . head . readHex <$> count 4 digit)
