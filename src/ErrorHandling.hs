{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ErrorHandling
--}

module ErrorHandling
    ( checkArgs,
    ) where

import Data.List (nub)

checkFormatArgs :: [String] -> Bool
checkFormatArgs [] = True
checkFormatArgs ("-f":val:rest) 
    | val `notElem` ["xml", "json", "markdown"] = False
    | otherwise = checkFormatArgs rest
checkFormatArgs ("-e":val:rest) 
    | val `notElem` ["xml", "json", "markdown"] = False
    | otherwise = checkFormatArgs rest
checkFormatArgs (_:rest) = checkFormatArgs rest

checkFlagRepeats :: [String] -> Bool
checkFlagRepeats args =
    let relevantFlags = filter (`elem` ["-i", "-f", "-e", "-o"]) args
    in length relevantFlags /= length (nub relevantFlags)

checkFlags :: [String] -> Bool
checkFlags [] = False
checkFlags args | notElem "-i" args || notElem "-f" args = False
    | head args `notElem` ["-i", "-f", "-o", "-e"] = False
    | length args >= 3 &&
        (args !! 2) `notElem` ["-i", "-f", "-o", "-e"] = False
    | length args >= 5 &&
        (args !! 4) `notElem` ["-i", "-f", "-o", "-e"] = False
    | length args == 7 &&
        (args !! 6) `notElem` ["-i", "-f", "-o", "-e"] = False
    | otherwise = True

checkArgs :: [String] -> Bool
checkArgs [] = False
checkArgs args
    | length args `notElem` [4, 6, 8] = False
    | not (checkFlags args) = False
    | checkFlagRepeats args = False
    | not (checkFormatArgs args) = False
    | otherwise = True
