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

printHeader :: ParserValue -> Int -> String
printHeader (ParserHead title Nothing Nothing) level =
    replicate (level * 4) ' ' ++ "\"header\": " ++ "{\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"title\": " ++
    toJson (ParserString title) 0 ++ "\n" ++
    replicate (level * 4) ' ' ++ "}"
printHeader (ParserHead title author Nothing) level =
    replicate (level * 4) ' ' ++ "\"header\": " ++ "{\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"title\": " ++
    toJson (ParserString title) 0 ++ ",\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"author\": " ++
    maybe "null" (\a -> toJson (ParserString a) 0) author ++ "\n" ++
    replicate (level * 4) ' ' ++ "}"
printHeader (ParserHead title Nothing date) level =
    replicate (level * 4) ' ' ++ "\"header\": " ++ "{\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"title\": " ++
    toJson (ParserString title) 0 ++ ",\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"date\": " ++
    maybe "null" (\d -> toJson (ParserString d) 0) date ++ "\n" ++
    replicate (level * 4) ' ' ++ "}"
printHeader (ParserHead title author date) level =
    replicate (level * 4) ' ' ++ "\"header\": " ++ "{\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"title\": " ++
    toJson (ParserString title) 0 ++ ",\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"author\": " ++
    maybe "null" (\a -> toJson (ParserString a) 0) author ++ ",\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"date\": " ++
    maybe "null" (\d -> toJson (ParserString d) 0) date ++ "\n" ++
    replicate (level * 4) ' ' ++ "}"
printHeader _ _ = ""

printItalic :: ParserValue -> Int -> String
printItalic (ParserItalic s) level = replicate (level * 4) ' ' ++ "{\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"italic\": \"" ++ s ++ "\"" ++ "\n" ++
    replicate (level * 4) ' ' ++ "}"
printItalic _ _ = ""


printBold :: ParserValue -> Int -> String
printBold (ParserBold s) level = replicate (level * 4) ' ' ++ "{\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"bold\": \"" ++ s ++ "\"" ++ "\n" ++
    replicate (level * 4) ' ' ++ "}"
printBold _ _ = ""

printCode :: ParserValue -> Int -> String
printCode (ParserCode s) level = replicate (level * 4) ' ' ++ "{\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"code\": \"" ++ s ++ "\"" ++ "\n" ++
    replicate (level * 4) ' ' ++ "}"
printCode _ _ = ""

printLink :: ParserValue -> Int -> String
printLink (ParserLink url content) level = replicate (level * 4) ' ' ++
    "{\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"link\": {\n" ++
    replicate (level * 4 + 8) ' ' ++ "\"url\": \"" ++ url ++ "\",\n" ++
    replicate (level * 4 + 8) ' ' ++ "\"content\": [\n" ++
    replicate (level * 4 + 12) ' ' ++ "\"" ++ content ++ "\"\n" ++
    replicate (level * 4 + 8) ' ' ++ "]\n" ++
    replicate (level * 4 + 4) ' ' ++ "}\n" ++
    replicate (level * 4) ' ' ++ "}"
printLink _ _ = ""

printImage :: ParserValue -> Int -> String
printImage (ParserImage url alt) level = replicate (level * 4) ' ' ++ "{\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"image\": {\n" ++
    replicate (level * 4 + 8) ' ' ++ "\"url\": \"" ++ url ++ "\",\n" ++
    replicate (level * 4 + 8) ' ' ++ "\"alt\": [\n" ++
    replicate (level * 4 + 12) ' ' ++ "\"" ++ alt ++ "\"\n" ++
    replicate (level * 4 + 8) ' ' ++ "]\n" ++
    replicate (level * 4 + 4) ' ' ++ "}\n" ++
    replicate (level * 4) ' ' ++ "}"
printImage _ _ = ""

printList :: ParserValue -> Int -> String
printList (ParserList items) level =
    replicate (level * 4) ' ' ++ "{\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"list\": [\n" ++
    intercalate ",\n" (map (\v -> toJson v (level + 2)) items) ++ "\n" ++
    replicate (level * 4 + 4) ' ' ++ "]\n" ++
    replicate (level * 4) ' ' ++ "}"
printList _ _ = ""

printCodeBlock :: ParserValue -> Int -> String
printCodeBlock (ParserCodeBlock codeLines) level =
    replicate (level * 4) ' ' ++ "{\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"codeblock\": [\n" ++
    intercalate ",\n" (map (`toJson` (level + 2)) codeLines) ++ "\n" ++
    replicate (level * 4 + 4) ' ' ++ "]\n" ++
    replicate (level * 4) ' ' ++ "}"
printCodeBlock _ _ = ""

printSection :: ParserValue -> Int -> String
printSection (ParserSection title content) level =
    replicate (level * 4) ' ' ++ "{\n" ++
    replicate (level * 4 + 4) ' ' ++ "\"section\": {\n" ++
    replicate (level * 4 + 8) ' ' ++ "\"title\": " ++
    toJson (ParserString title) 0 ++ ",\n" ++
    replicate (level * 4 + 8) ' ' ++ "\"content\": [\n" ++
    intercalate ",\n" (map (\v -> toJson v (level + 3)) content) ++
    "\n" ++ replicate (level * 4 + 8) ' ' ++ "]\n" ++
    replicate (level * 4 + 4) ' ' ++ "}\n" ++
    replicate (level * 4) ' ' ++ "}"
printSection _ _ = ""

printParagraph :: ParserValue -> Int -> String
printParagraph (ParserParagraph content) level =
    replicate (level * 4) ' ' ++ "[\n" ++
    intercalate ",\n" (map (\v -> toJson v (level + 1)) content) ++ "\n" ++
    replicate (level * 4) ' ' ++ "]"
printParagraph _ _ = ""

printBody :: ParserValue -> Int -> String
printBody (ParserBody content) level =
    replicate (level * 4) ' ' ++ "\"body\": [\n" ++
    intercalate ",\n" (map (\v -> toJson v (level + 1)) content) ++ "\n" ++
    replicate (level * 4) ' ' ++ "]"
printBody _ _ = ""

printArray :: ParserValue -> Int -> String
printArray (ParserArray items) level =
    "[\n" ++ intercalate ",\n" (map (\v -> toJson v (level + 1)) items) ++
    "\n" ++ replicate (level * 4) ' ' ++ "]"
printArray _ _ = ""

toJson :: ParserValue -> Int -> String
toJson (ParserString s) level = replicate (level * 4) ' ' ++ "\"" ++ s ++ "\""
toJson (ParserItalic s) level = printItalic (ParserItalic s) level
toJson (ParserBold s) level = printBold (ParserBold s) level
toJson (ParserCode s) level = printCode (ParserCode s) level
toJson (ParserLink url content) level =
    printLink (ParserLink url content) level
toJson (ParserImage url alt) level = printImage (ParserImage url alt) level
toJson (ParserList items) level = printList (ParserList items) level
toJson (ParserCodeBlock codeLines) level =
    printCodeBlock (ParserCodeBlock codeLines) level
toJson (ParserSection title content) level =
    printSection (ParserSection title content) level
toJson (ParserParagraph content) level =
    printParagraph (ParserParagraph content) level
toJson (ParserHead title author date) level =
    printHeader (ParserHead title author date) level
toJson (ParserBody content) level = printBody (ParserBody content) level
toJson (ParserArray items) level = printArray (ParserArray items) level
toJson (ParserObject pairs) level =
    "{\n" ++ intercalate ",\n" (map (\v -> toJson v (level + 1)) pairs)
    ++ "\n" ++ replicate (level * 4) ' ' ++ "}"

-- If no file path is provided, print in terminal instead
writeJsonFile :: Maybe FilePath -> ParserValue -> IO ()
writeJsonFile Nothing value = putStrLn (toJson value 0)
writeJsonFile (Just path) value = writeFile path (toJson value 0)