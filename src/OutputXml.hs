{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- OutputXml
--}

module OutputXml
    ( writeXmlFile
    ) where

import Parser ( ParserValue(..) )
import Data.List ( intercalate )

toXml :: ParserValue -> Int -> String
toXml (ParserString s) _ = escapeXml s
toXml (ParserArray arr) level =
    "<array>\n" ++ indentedValues ++ "\n" ++
        replicate (level * 4) ' ' ++ "</array>"
    where
        indentedValues = concatMap (\v -> toXml v (level + 1)) arr

toXml (ParserObject obj) level =
    "<section>\n" ++ indentedPairs ++ "\n" ++
        replicate (level * 4) ' ' ++ "</section>"
    where
        indentedPairs = concatMap (\(k, v) -> replicate ((level + 1) * 4) ' '
            ++ "<" ++ k ++ ">" ++ toXml v (level + 1) ++ "</" ++ k ++ ">") obj

escapeXml :: String -> String
escapeXml = concatMap escapeChar
    where
        escapeChar '<' = "&lt;"
        escapeChar '>' = "&gt;"
        escapeChar '&' = "&amp;"
        escapeChar '"' = "&quot;"
        escapeChar '\'' = "&apos;"
        escapeChar c = [c]


writeXmlFile :: Maybe FilePath -> ParserValue -> IO ()
writeXmlFile Nothing _ = putStrLn "No file path provided."
writeXmlFile (Just path) value = writeFile path (toXml value 0)
