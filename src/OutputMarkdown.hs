{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- OutputMarkdown
--}

module OutputMarkdown
    ( writeMarkdownFile
    ) where

import Data.Maybe()
import Parser ( ParserValue(..) )

printMdHeader :: ParserValue -> String
printMdHeader (ParserHead title Nothing Nothing) =
    "---\n" ++ "title: " ++ escapeMarkdown title ++ "\n" ++ "---\n\n"
printMdHeader (ParserHead title author Nothing) =
    "---\n" ++ "title: " ++ escapeMarkdown title ++ "\n" ++
    "author: " ++ maybe "" escapeMarkdown author ++ "\n" ++ "---\n\n"
printMdHeader (ParserHead title Nothing date) =
    "---\n" ++ "title: " ++ escapeMarkdown title ++ "\n" ++
    "date: " ++ maybe "" escapeMarkdown date ++ "\n" ++ "---\n\n"
printMdHeader (ParserHead title author date) =
    "---\n" ++ "title: " ++ escapeMarkdown title ++ "\n" ++
    "date: " ++ maybe "" escapeMarkdown date ++ "\n" ++
    "author: " ++ maybe "" escapeMarkdown author ++ "\n" ++ "---\n\n"
printMdHeader _ = ""

printSection :: ParserValue -> Int -> String
printSection (ParserSection "" items) indentLevel =
    concatMap (\item -> toMarkdown item (indentLevel + 1)) items
printSection (ParserSection title items) indentLevel =
    "\n" ++ replicate (indentLevel + 1) '#' ++ " " ++ title ++ "\n\n" ++
    concatMap (\item -> toMarkdown item (indentLevel + 1)) items
printSection _ _ = ""


toMarkdown :: ParserValue -> Int -> String
toMarkdown (ParserHead title maybeAuthor maybeDate) _ =
    printMdHeader (ParserHead title maybeAuthor maybeDate)
toMarkdown (ParserString s) _ = escapeMarkdown s
toMarkdown (ParserItalic s) _ = "*" ++ escapeMarkdown s ++ "*"
toMarkdown (ParserBold s) _ = "**" ++ escapeMarkdown s ++ "**"
toMarkdown (ParserCode s) _ = "`" ++ escapeMarkdown s ++ "`"
toMarkdown (ParserLink url value) _ =
    "[" ++ escapeMarkdown value ++ "](" ++ escapeMarkdown url ++ ")"
toMarkdown (ParserImage url value) _ =
    "![" ++ escapeMarkdown value ++ "](" ++ escapeMarkdown url ++ ")"
toMarkdown (ParserList items) _ =
    concatMap (\item -> "- " ++ toMarkdown item 0) items ++ "\n"
toMarkdown (ParserCodeBlock items) _ =
    "```\n" ++ concatMap (`toMarkdown` 0) items ++ "```" ++ "\n"
toMarkdown (ParserSection title items) indentLevel =
    printSection (ParserSection title items) indentLevel
toMarkdown (ParserParagraph items) indentLevel =
    concatMap (\item -> toMarkdown item indentLevel) items ++ "\n"
toMarkdown (ParserBody items) indentLevel =
    concatMap (\item -> toMarkdown item indentLevel) items
toMarkdown (ParserArray items) indentLevel =
    concatMap (\item -> toMarkdown item indentLevel) items
toMarkdown (ParserObject items) indentLevel =
    concatMap (\item -> toMarkdown item indentLevel) items

escapeMarkdown :: String -> String
escapeMarkdown = concatMap escapeChar
    where
        escapeChar '<' = "&lt;"
        escapeChar '>' = "&gt;"
        escapeChar '&' = "&amp;"
        escapeChar '"' = "&quot;"
        escapeChar '\'' = "'"
        escapeChar c = [c]

writeMarkdownFile :: Maybe FilePath -> ParserValue -> IO ()
writeMarkdownFile Nothing value = putStrLn $ toMarkdown value 0
writeMarkdownFile (Just path) value = writeFile path (toMarkdown value 0)