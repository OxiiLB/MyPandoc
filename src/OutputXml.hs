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

toXml :: ParserValue -> Int -> String
toXml (ParserString s) indentLevel = replicate (indentLevel * 4) ' ' ++ "<paragraph>" ++ escapeXml s ++ "</paragraph>\n"
toXml (ParserItalic s) indentLevel = replicate (indentLevel * 4) ' ' ++ "<italic>" ++ escapeXml s ++ "</italic>\n"
toXml (ParserBold s) indentLevel = replicate (indentLevel * 4) ' ' ++ "<bold>" ++ escapeXml s ++ "</bold>\n"
toXml (ParserCode s) indentLevel = replicate (indentLevel * 4) ' ' ++ "<code>" ++ escapeXml s ++ "</code>\n"
toXml (ParserLink url value) indentLevel = replicate (indentLevel * 4) ' ' ++ "<link url=\"" ++ url ++ "\">" ++ escapeXml value ++ "</link>\n"
toXml (ParserImage url value) indentLevel = replicate (indentLevel * 4) ' ' ++ "<image url=\"" ++ url ++ "\">" ++ escapeXml value ++ "</image>\n"
toXml (ParserList items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<list>\n" ++ innerXml items (indentLevel + 1) ++ replicate (indentLevel * 4) ' ' ++ "</list>\n"
toXml (ParserCodeBlock items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<codeblock>\n" ++ innerXml items (indentLevel + 1) ++ replicate (indentLevel * 4) ' ' ++ "</codeblock>\n"
toXml (ParserSection title items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<section title=\"" ++ escapeXml title ++ "\">\n" ++ innerXml items (indentLevel + 1) ++ replicate (indentLevel * 4) ' ' ++ "</section>\n"
toXml (ParserParagraph items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<paragraph>" ++ innerText items ++ "</paragraph>\n"
toXml (ParserHead title maybeAuthor maybeDate) indentLevel =
    replicate (indentLevel * 4) ' ' ++ "<header title=\"" ++ escapeXml title ++ "\">\n" ++ authorAttr ++ dateAttr ++ replicate (indentLevel * 4) ' ' ++ "</header>\n"
    where
        authorAttr = maybe "" (\author -> replicate ((indentLevel + 1) * 4) ' ' ++ "<author>" ++ escapeXml author ++ "</author>\n") maybeAuthor
        dateAttr = maybe "" (\date -> replicate ((indentLevel + 1) * 4) ' ' ++ "<date>" ++ escapeXml date ++ "</date>\n") maybeDate
toXml (ParserBody items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<body>\n" ++ innerXml items (indentLevel + 1) ++ replicate (indentLevel * 4) ' ' ++ "</body>\n"
toXml (ParserArray items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<array>\n" ++ innerXml items (indentLevel + 1) ++ replicate (indentLevel * 4) ' ' ++ "</array>\n"
toXml (ParserObject items) indentLevel = replicate (indentLevel * 4) ' ' ++ "<document>\n" ++ innerXml items (indentLevel + 1) ++ replicate (indentLevel * 4) ' ' ++ "</document>\n"

innerXml :: [ParserValue] -> Int -> String
innerXml items indentLevel = concatMap (\item -> toXml item indentLevel) items

innerText :: [ParserValue] -> String
innerText [] = ""
innerText (ParserString s : xs) = escapeXml s ++ innerText xs
innerText (ParserItalic s : xs) = "<italic>" ++ escapeXml s ++ "</italic>" ++ innerText xs
innerText (ParserBold s : xs) = "<bold>" ++ escapeXml s ++ "</bold>" ++ innerText xs
innerText (ParserCode s : xs) = "<code>" ++ escapeXml s ++ "</code>" ++ innerText xs
innerText (ParserLink url value : xs) = "<link url=\"" ++ url ++ "\">" ++ escapeXml value ++ "</link>" ++ innerText xs
innerText (ParserImage url value : xs) = "<image url=\"" ++ url ++ "\">" ++ escapeXml value ++ "</image>" ++ innerText xs
innerText (x:xs) = innerText xs


escapeXml :: String -> String
escapeXml = concatMap escapeChar
    where
        escapeChar '<' = "&lt;"
        escapeChar '>' = "&gt;"
        escapeChar '&' = "&amp;"
        escapeChar '"' = "&quot;"
        escapeChar '\'' = "'"
        escapeChar c = [c]

writeXmlFile :: Maybe FilePath -> ParserValue -> IO ()
writeXmlFile Nothing value = putStrLn $ toXml value 0
writeXmlFile (Just path) value = writeFile path (toXml value 0)
