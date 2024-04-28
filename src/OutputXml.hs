{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- OutputXml
--}

module OutputXml
    ( -- writeXmlFile
    ) where

import Parser ( ParserValue(..) )

-- toXml :: ParserValue -> Int -> String
-- toXml (ParserString s) indentLevel = replicate (indentLevel * 4) ' ' ++ escapeXml s ++ "\n"
-- toXml (ParserItalic s) indentLevel = replicate (indentLevel * 4) ' ' ++ "<italic>" ++ escapeXml s ++ "</italic>\n"
-- toXml (ParserBold s) indentLevel = replicate (indentLevel * 4) ' ' ++ "<bold>" ++ escapeXml s ++ "</bold>\n"
-- toXml (ParserCode s) indentLevel = replicate (indentLevel * 4) ' ' ++ "<code>" ++ escapeXml s ++ "</code>\n"
-- toXml (ParserLink url value) indentLevel = replicate (indentLevel * 4) ' ' ++ "<link url=\"" ++ url ++ "\">" ++ escapeXml value ++ "</link>\n"
-- toXml (ParserImage url value) indentLevel = replicate (indentLevel * 4) ' ' ++ "<image url=\"" ++ url ++ "\">" ++ escapeXml value ++ "</image>\n"
-- toXml (ParserList items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<list>\n" ++ concatMap (\item -> toXml item (indentLevel + 1)) items ++ replicate (indentLevel * 4) ' ' ++ "</list>\n"
-- toXml (ParserCodeBlock items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<codeblock>\n" ++ concatMap (\item -> toXml item (indentLevel + 1)) items ++ replicate (indentLevel * 4) ' ' ++ "</codeblock>\n"
-- toXml (ParserSection title items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<section title=\"" ++ escapeXml title ++ "\">\n" ++ concatMap (\item -> toXml item (indentLevel + 1)) items ++ replicate (indentLevel * 4) ' ' ++ "</section>\n"
-- toXml (ParserParagraphe items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<paragraph>\n" ++ concatMap (\item -> toXml item (indentLevel + 1)) items ++ replicate (indentLevel * 4) ' ' ++ "</paragraph>\n"
-- toXml (ParserHead title maybeAuthor maybeDate) indentLevel =
--     replicate (indentLevel * 4) ' ' ++ "<header title=\"" ++ escapeXml title ++ "\"" ++ authorAttr ++ dateAttr ++ "/>\n"
--     where
--         authorAttr = maybe "" (\author -> " author=\"" ++ escapeXml author ++ "\"") maybeAuthor
--         dateAttr = maybe "" (\date -> " date=\"" ++ escapeXml date ++ "\"") maybeDate
-- toXml (ParserBody items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<body>\n" ++ concatMap (\item -> toXml item (indentLevel + 1)) items ++ replicate (indentLevel * 4) ' ' ++ "</body>\n"
-- toXml (ParserArray items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<array>\n" ++ concatMap (\item -> toXml item (indentLevel + 1)) items ++ replicate (indentLevel * 4) ' ' ++ "</array>\n"
-- toXml (ParserObject items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<object>\n" ++ concatMap (\item -> toXml item (indentLevel + 1)) items ++ replicate (indentLevel * 4) ' ' ++ "</object>\n"


-- escapeXml :: String -> String
-- escapeXml = concatMap escapeChar
--     where
--         escapeChar '<' = "&lt;"
--         escapeChar '>' = "&gt;"
--         escapeChar '&' = "&amp;"
--         escapeChar '"' = "&quot;"
--         escapeChar '\'' = "&apos;"
--         escapeChar c = [c]


-- writeXmlFile :: Maybe FilePath -> ParserValue -> IO ()
-- writeXmlFile Nothing value = putStrLn $ toXml value 0
-- writeXmlFile (Just path) value = writeFile path (toXml value 0)
