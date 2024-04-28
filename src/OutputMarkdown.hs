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

toMarkdown :: ParserValue -> Int -> String
toMarkdown (ParserHead title maybeAuthor maybeDate) _ =
    "---\n" ++ "title: " ++ escapeMarkdown title ++ "\n" ++
    "author: " ++ maybe "" escapeMarkdown maybeAuthor ++ "\n" ++
    "date: " ++ maybe "" escapeMarkdown maybeDate ++ "\n" ++
    "---\n\n"
toMarkdown (ParserString s) _ = escapeMarkdown s ++ "\n"
toMarkdown (ParserItalic s) _ = "*" ++ escapeMarkdown s ++ "*"
toMarkdown (ParserBold s) _ = "**" ++ escapeMarkdown s ++ "**"
toMarkdown (ParserCode s) _ = "`" ++ escapeMarkdown s ++ "`"
toMarkdown (ParserLink url value) _ = "[" ++ escapeMarkdown value ++ "](" ++ escapeMarkdown url ++ ")"
toMarkdown (ParserImage url _) _ = "![Text to replace image](" ++ escapeMarkdown url ++ ")"
toMarkdown (ParserList items) indentLevel = concatMap (\item -> replicate (indentLevel * 2) ' ' ++ "- " ++ toMarkdown item 0 ++ "\n") items
toMarkdown (ParserCodeBlock items) indentLevel = replicate (indentLevel * 4) ' ' ++ "```\n" ++ concatMap (\item -> replicate (indentLevel * 4) ' ' ++ toMarkdown item 0 ++ "\n") items ++ replicate (indentLevel * 4) ' ' ++ "```\n"
toMarkdown (ParserSection title items) indentLevel = replicate (indentLevel + 1) '#' ++ " " ++ escapeMarkdown title ++ "\n" ++ concatMap (\item -> toMarkdown item (indentLevel + 1)) items ++ "\n"
toMarkdown (ParserParagraph items) indentLevel = concatMap (\item -> toMarkdown item indentLevel) items ++ "\n"
toMarkdown (ParserBody items) indentLevel = concatMap (\item -> toMarkdown item indentLevel) items ++ "\n"
toMarkdown (ParserArray items) indentLevel = concatMap (\item -> toMarkdown item indentLevel) items ++ "\n"
toMarkdown (ParserObject items) indentLevel = concatMap (\item -> toMarkdown item indentLevel) items ++ "\n"

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