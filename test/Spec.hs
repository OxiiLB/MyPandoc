{--
-- EPITECH PROJECT, 2024
-- Tek2
-- File description:
-- test
--}
import Test.HUnit
import Parser
import JsonParser
import XmlParser

-- parseCharTest
testParseChar1:: Test
testParseChar1 = TestCase (assertEqual "for parseChar,"
    (runParser (parseChar 'a') input) (Just ('a', "bc")))
  where
    input = "abc"

testParseChar2:: Test
testParseChar2 = TestCase (assertEqual "for parseChar,"
    (runParser (parseChar 'a') input) (Nothing))
  where
    input = "bc"

-- parseAnyCharTest
testParseAnyChar1:: Test
testParseAnyChar1 = TestCase (assertEqual "for parseAnyChar,"
    (runParser (parseAnyChar "ab") input) (Just ('a', "bc")))
  where
    input = "abc"

testParseAnyChar2:: Test
testParseAnyChar2 = TestCase (assertEqual "for parseAnyChar,"
    (runParser (parseAnyChar "ab") input) (Just ('b', "c")))
  where
    input = "bc"

testParseAnyChar3:: Test
testParseAnyChar3 = TestCase (assertEqual "for parseAnyChar,"
    (runParser (parseAnyChar "ab") input) (Nothing))
  where
    input = "c"

-- parseStrTest
testParseStr1:: Test
testParseStr1 = TestCase (assertEqual "for parseStr,"
    (runParser (parseStr "abc") input) (Just ("abc", "")))
  where
    input = "abc"

testParseStr2:: Test
testParseStr2 = TestCase (assertEqual "for parseStr,"
    (runParser (parseStr "abc") input) (Nothing))
  where
    input = "bc"

-- parseStringQuotedTest
testParseStringQuoted1:: Test
testParseStringQuoted1 = TestCase (assertEqual "for parseStringQuoted,"
    (runParser parseStringQuoted input) (Just ("abc", "")))
  where
    input = "\"abc\""

testParseStringQuoted2:: Test
testParseStringQuoted2 = TestCase (assertEqual "for parseStringQuoted,"
    (runParser parseStringQuoted input) (Nothing))
  where
    input = "abc"

-- parseOrTest
testParseOr1:: Test
testParseOr1 = TestCase (assertEqual "for parseOr,"
    (runParser (parseOr (parseChar 'a') (parseChar 'b')) input) (Just ('a', "bc")))
  where
    input = "abc"

testParseOr2:: Test
testParseOr2 = TestCase (assertEqual "for parseOr,"
    (runParser (parseOr (parseChar 'a') (parseChar 'b')) input) (Just ('b', "c")))
  where
    input = "bc"

-- parseAndTest
testParseAnd1:: Test
testParseAnd1 = TestCase (assertEqual "for parseAnd,"
    (runParser (parseAnd (parseChar 'a') (parseChar 'b')) input)
    (Just (('a', 'b'), "c")))
  where
    input = "abc"

testParseAnd2:: Test
testParseAnd2 = TestCase (assertEqual "for parseAnd,"
    (runParser (parseAnd (parseChar 'a') (parseChar 'b')) input) (Nothing))
  where
    input = "bc"

-- parseAndWithTest
testParseAndWith1:: Test
testParseAndWith1 = TestCase (assertEqual "for parseAndWith,"
    (runParser (parseAndWith (+) parseInt parseInt) input) (Nothing))
  where
    input = "1c"

testParseAndWith2:: Test
testParseAndWith2 = TestCase (assertEqual "for parseAndWith,"
    (runParser (parseAndWith (+) parseInt parseInt) input) (Nothing))
  where
    input = "ac"

-- parseManyTest
testParseMany1:: Test
testParseMany1 = TestCase (assertEqual "for parseMany,"
    (runParser (parseMany (parseChar 'a')) input) (Just ("aaa", "")))
  where
    input = "aaa"

testParseMany2:: Test
testParseMany2 = TestCase (assertEqual "for parseMany,"
    (runParser (parseMany (parseChar 'a')) input) (Just ("a", "b")))
  where
    input = "ab"

-- parseIntTest
testParseInt1:: Test
testParseInt1 = TestCase (assertEqual "for parseInt,"
    (runParser parseInt input) (Just (123, "")))
  where
    input = "123"

testParseInt2:: Test
testParseInt2 = TestCase (assertEqual "for parseInt,"
    (runParser parseInt input) (Just (123, "abc")))
  where
    input = "123abc"

-- parseUIntTest
testParseUInt1:: Test
testParseUInt1 = TestCase (assertEqual "for parseUInt,"
    (runParser parseUInt input) (Just (123, "")))
  where
    input = "123"

testParseUInt2:: Test
testParseUInt2 = TestCase (assertEqual "for parseUInt,"
    (runParser parseUInt input) (Just (123, "abc")))
  where
    input = "123abc"

-- parseTupleTest
testParseTuple1:: Test
testParseTuple1 = TestCase (assertEqual "for parseTuple,"
    (runParser (parseTuple parseInt) input) (Just ((1, 2), "")))
  where
    input = "1,2"

testParseTuple2:: Test
testParseTuple2 = TestCase (assertEqual "for parseTuple,"
    (runParser (parseTuple parseInt) input) (Nothing))
  where
    input = "1a2"

-- parseJsonSectionTest
testParseJsonSection1:: Test
testParseJsonSection1 = TestCase (assertEqual "for parseJsonSection,"
    (runParser parserJsonSection input) (Nothing))
  where
    input = "{\"section\":{\"title\":\"section\",\"content\":[\"content\"]}"

testParseJsonSection2:: Test
testParseJsonSection2 = TestCase (assertEqual "for parseJsonSection,"
    (runParser parserJsonSection input) (Just (ParserSection "section"
    [ParserString "content"],"")))
  where
    input = "{\"section\":{\"title\":\"section\",\"content\":[\"content\"]}}"

-- parseJsonHeaderTest
testParseJsonHeader1:: Test
testParseJsonHeader1 = TestCase (assertEqual "for parseJsonHeader,"
    (runParser parserJsonHeader input) (Nothing))
  where
    input = "{\"header\":{\"title\":\"header\","
         ++ "\"author\":\"author\","
         ++ "\"date\":\"date\"}}"

testParseJsonHeader2:: Test
testParseJsonHeader2 = TestCase (assertEqual "for parseJsonHeader,"
    (runParser parserJsonHeader input) (Nothing))
  where
    input = "{\"header\":{\"title\":\"header\","
         ++ "\"author\":\"author\"},"
         ++ "\"date\":\"date\"}}"

-- testParseJsonValueTest
testParseJsonValue1:: Test
testParseJsonValue1 = TestCase (assertEqual "for parseJsonValue,"
    (runParser parseJsonValue input) (Just (ParserString "string", "")))
  where
    input = "\"string\""

testParseJsonValue2:: Test
testParseJsonValue2 = TestCase (assertEqual "for parseJsonValue,"
    (runParser parseJsonValue input) (Just (ParserParagraph
    [ParserString "string"], "")))
  where
    input = "[\"string\"]"

testParseJsonValue3:: Test
testParseJsonValue3 = TestCase (assertEqual "for parseJsonValue,"
    (runParser parseJsonValue input) (Just (ParserSection "section"
    [ParserString "content"],"")))
  where
    input = "{\"section\":{\"title\":\"section\",\"content\":[\"content\"]}}"

-- testParseXmlValue
testParseXmlValue1:: Test
testParseXmlValue1 = TestCase (assertEqual "for parseXmlValue,"
    (runParser parseXmlValue input) (Just (ParserString "","\"string\"")))
  where
    input = "\"string\""

testParseXmlValue2:: Test
testParseXmlValue2 = TestCase (assertEqual "for parseXmlValue,"
    (runParser parseXmlValue input) (Just (ParserParagraph
    [ParserString "string"], "")))
  where
    input = "<paragraph>string</paragraph>"

testParseXmlValue3:: Test
testParseXmlValue3 = TestCase (assertEqual "for parseXmlValue,"
    (runParser parseXmlValue input) (Just (ParserObject
    [ParserSection "section" [ParserString "content"]],"")))
  where
    input = "<document><section title=\"section\">content</section></document>"

-- tests List
parseCharTests :: Test
parseCharTests = TestList [testParseChar1, testParseChar2]

parseAnyCharTests :: Test
parseAnyCharTests = TestList [testParseAnyChar1, testParseAnyChar2,
    testParseAnyChar3]

parseStrTests :: Test
parseStrTests = TestList [testParseStr1, testParseStr2]

parseStringQuotedTests :: Test
parseStringQuotedTests = TestList [testParseStringQuoted1,
    testParseStringQuoted2]

parseOrTests :: Test
parseOrTests = TestList [testParseOr1, testParseOr2]

parseAndTests :: Test
parseAndTests = TestList [testParseAnd1, testParseAnd2]

parseAndWithTests :: Test
parseAndWithTests = TestList [testParseAndWith1, testParseAndWith2]

parseManyTests :: Test
parseManyTests = TestList [testParseMany1, testParseMany2]

parseIntTests :: Test
parseIntTests = TestList [testParseInt1, testParseInt2]

parseUIntTests :: Test
parseUIntTests = TestList [testParseUInt1, testParseUInt2]

parseTupleTests :: Test
parseTupleTests = TestList [testParseTuple1, testParseTuple2]

parseJsonSectionTests :: Test
parseJsonSectionTests = TestList [testParseJsonSection1, testParseJsonSection2]

parseJsonHeaderTests :: Test
parseJsonHeaderTests = TestList [testParseJsonHeader1, testParseJsonHeader2]

parseJsonValueTests :: Test
parseJsonValueTests = TestList [testParseJsonValue1, testParseJsonValue2,
    testParseJsonValue3]
  
parseXmlValueTests :: Test
parseXmlValueTests = TestList [testParseXmlValue1, testParseXmlValue2,
    testParseXmlValue3]

tests :: Test
tests = TestList [parseCharTests, parseAnyCharTests, parseStrTests,
    parseStringQuotedTests, parseOrTests, parseAndTests, parseAndWithTests,
    parseManyTests, parseIntTests, parseUIntTests, parseTupleTests,
    parseJsonSectionTests, parseJsonHeaderTests, parseJsonValueTests,
    parseXmlValueTests]

main :: IO Counts
main = runTestTT tests
