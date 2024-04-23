import Control.Applicative (Alternative(..), optional)
import Data.Char (chr)
import Numeric (readHex)

-- Define your Parser type and its associated functions here
newtype Parser a = Parser {
    runParser :: String -> Maybe (a, String)
}

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (x, rest) <- p input
        pure (f x, rest)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (x, input)
    (Parser pf) <*> (Parser px) = Parser $ \input -> do
        (f, rest1) <- pf input
        (x, rest2) <- px rest1
        pure (f x, rest2)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input ->
        p1 input <|> p2 input

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \input -> case input of
    (c:cs) | f c -> Just (c, cs)
    _            -> Nothing

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
    plus = satisfy (== '+') *> num
    minus = (:) <$> satisfy (== '-') <*> num
    num = some digit
    decimal = (++) <$> num <*> ((:) <$> satisfy (== '.') <*> num)
    exponent = (\e sign exp -> let multiplier = case sign of
                                          Just '+' -> 1
                                          _ -> -1
                           in "e" ++ [e] ++ show (multiplier * read exp))
              <$> satisfy (`elem` "eE")
              <*> optional (satisfy (`elem` "+-"))
              <*> num

-- parseQuotedString parser
parseQuotedString :: Parser String
parseQuotedString = satisfy (== '"') *> many (escapedChar <|> satisfy (/= '"')) <* satisfy (== '"')
  where
    escapedChar = satisfy (== '\\') *> choice
        [ '"'  <$ satisfy (== '"')
        , '\\' <$ satisfy (== '\\')
        , '/'  <$ satisfy (== '/')
        , '\b' <$ satisfy (== 'b')
        , '\f' <$ satisfy (== 'f')
        , '\n' <$ satisfy (== 'n')
        , '\r' <$ satisfy (== 'r')
        , '\t' <$ satisfy (== 't')
        , '\v' <$ satisfy (== 'v')
        , '&'  <$ string "&amp;" -- XML escape for "&"
        , '>'  <$ string "&gt;"  -- XML escape for ">"
        , '<'  <$ string "&lt;"  -- XML escape for "<"
        , '"'  <$ string "&quot;"-- XML escape for """
        , '\'' <$ string "&apos;"-- XML escape for "'"
        , hexChar
        ]
    hexChar = satisfy (== 'u') *> (chr . fromInteger . fst . head . readHex <$> count 4 digit)

-- Count combinator
count :: Int -> Parser a -> Parser [a]
count n p
    | n <= 0    = pure []
    | otherwise = (:) <$> p <*> count (n - 1) p

-- Choice combinator
choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty

-- String parser
string :: String -> Parser String
string []     = pure []
string (x:xs) = (:) <$> satisfy (== x) <*> string xs

-- Digits parser
digit :: Parser Char
digit = satisfy (`elem` "0123456789")
