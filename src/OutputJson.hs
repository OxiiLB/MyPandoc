{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- OutputJson
--}

module OutputJson
    ( writeJsonFile
    ) where

import Parser ( ParserValue(..) )
import Data.List ( intercalate )

toJson :: ParserValue -> Int -> String
toJson (ParserString s) _ = "\"" ++ s ++ "\""
toJson (ParserArray arr) level =
    "[\n" ++ indentedValues ++ "\n" ++ replicate (level * 4) ' ' ++ "]"
    where
        indentedValues = intercalate ",\n" $ map
            (\v -> replicate ((level + 1) * 4) ' ' ++ toJson v (level + 1)) arr

toJson (ParserObject obj) level =
    "{\n" ++ indentedPairs ++ "\n" ++ replicate (level * 4) ' ' ++ "}"
    where
        indentedPairs = intercalate ",\n" $ map (\(k, v) ->
            replicate((level + 1) * 4) ' ' ++ "\"" ++ k ++ "\": " ++
                toJson v (level + 1)) obj


writeJsonFile :: FilePath -> ParserValue -> IO ()
writeJsonFile path value = writeFile path (toJson value 0)