{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- OutputMarkdown
--}

module OutputMarkdown
    ( writeMarkdownFile
    ) where

import Parser ( ParserValue(..) )
import Data.List ( intercalate )

toMarkdown :: ParserValue -> Int -> String
toMarkdown (ParserString s) _ = s
toMarkdown (ParserArray arr) _ = "-" ++
    intercalate "\n-" (map (`toMarkdown` 0) arr)

toMarkdown (ParserObject obj) level = intercalate "\n" $ map (\(k, v) ->
    replicate (level * 4) ' ' ++ "- " ++ k ++ ":\n"
        ++ toMarkdown v (level + 1)) obj

writeMarkdownFile :: Maybe FilePath -> ParserValue -> IO ()
writeMarkdownFile Nothing _ = putStrLn "No file path provided."
writeMarkdownFile (Just path) value = writeFile path (toMarkdown value 0)