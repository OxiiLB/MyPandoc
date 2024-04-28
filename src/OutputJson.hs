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
import GHC.IO.Device (RawIO(write))

toJson :: ParserValue -> Int -> String
toJson (ParserString s) _ = "\"" ++ s ++ "\""
toJson (ParserItalic s) _ = "*_" ++ s ++ "_*"
toJson (ParserBold s) _ = "**" ++ s ++ "**"
toJson (ParserCode s) _ = "`" ++ s ++ "`"
toJson (ParserLink url content) _ = "[" ++ content ++ "](" ++ url ++ ")"
toJson (ParserImage url alt) _ = "![" ++ alt ++ "](" ++ url ++ ")"
toJson (ParserList items) level =
    replicate (level * 4) ' ' ++ "- " ++ intercalate "\n" (map (\v -> toJson v 0) items)
toJson (ParserCodeBlock lines) level =
    replicate (level * 4) ' ' ++ "```\n" ++ intercalate "\n" (map (\v -> toJson v 0) lines) ++ "\n" ++ replicate (level * 4) ' ' ++ "```"
toJson (ParserSection title content) level =
    replicate (level * 4) ' ' ++ "## " ++ title ++ "\n" ++ intercalate "\n" (map (\v -> toJson v (level + 1)) content)
toJson (ParserParagraph content) level =
    replicate (level * 4) ' ' ++ intercalate "\n" (map (\v -> toJson v 0) content)
toJson (ParserHead title author date) _ =
    "{\n" ++
    replicate 4 ' ' ++ "\"title\": " ++ toJson (ParserString title) 0 ++ ",\n" ++
    replicate 4 ' ' ++ "\"author\": " ++ maybe "null" (\a -> toJson (ParserString a) 0) author ++ ",\n" ++
    replicate 4 ' ' ++ "\"date\": " ++ maybe "null" (\d -> toJson (ParserString d) 0) date ++ "\n" ++
    "}\n"
toJson (ParserBody content) level =
    intercalate "\n" (map (\v -> toJson v level) content)
toJson (ParserArray items) level =
    "[\n" ++ intercalate ",\n" (map (\v -> toJson v (level + 1)) items) ++ "\n" ++ replicate (level * 4) ' ' ++ "]"
toJson (ParserObject pairs) level =
    "{\n" ++ intercalate ",\n" (map (\v -> toJson v (level + 1)) pairs) ++ "\n" ++ replicate (level * 4) ' ' ++ "}"

-- If no file path is provided, print in terminal instead
writeJsonFile :: Maybe FilePath -> ParserValue -> IO ()
writeJsonFile Nothing value = putStrLn (toJson value 0)
writeJsonFile (Just path) value = writeFile path (toJson value 0)