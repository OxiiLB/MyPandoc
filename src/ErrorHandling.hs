{--
-- EPITECH PROJECT, 2024
-- MyPandoc
-- File description:
-- ErrorHandling
--}

module ErrorHandling
    ( checkArgs,
    ) where

-- check flag values
-- (args !! 1 /= "xml") && (args !! 1 /= "json") && (args !! 1 /= "markdown") = False

checkFlags :: [String] -> Bool
checkFlags [] = False
checkFlags args | not ("-i" `elem` args) || not ("-f" `elem` args) = False
    | (args !! 0 /= "-i" && args !! 0 /= "-f" && args !! 0 /= "-o"
        && args !! 0 /= "-e") = False
    | (args !! 2 /= "-i" && args !! 2 /= "-f" && args !! 2 /= "-o"
        && args !! 2 /= "-e") = False
    | (length args >= 5 && (args !! 4 /= "-i" && args !! 4 /= "-f"
        && args !! 4 /= "-o" && args !! 4 /= "-e")) = False
    | (length args >= 7 && (args !! 6 /= "-i" && args !! 6 /= "-f"
        && args !! 6 /= "-o" && args !! 6 /= "-e")) = False
    | otherwise = True

checkArgs :: [String] -> Bool
checkArgs [] = False
checkArgs args
    | length args < 4 || length args > 7 = False
    | checkFlags args == False = False
    | otherwise = True
