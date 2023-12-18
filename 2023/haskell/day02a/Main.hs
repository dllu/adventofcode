module Main ( main ) where

import Data.List
import Data.Maybe
import System.Environment
import System.Exit

usage :: IO ()
usage =
    do
        progName <- getProgName    
        putStrLn ("usage: " ++ progName ++ " <file>")
        exitFailure

splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn delimiter (x:xs)
  | x == delimiter = "" : rest
  | otherwise = (x : head rest) : tail rest
  where
    rest = splitOn delimiter xs

trimLeft :: String -> String
trimLeft [] = []
trimLeft (x:xs)
    | x == ' ' || x == '\t' || x == '\n' = trimLeft xs
    | otherwise = x : xs

checkColors :: String -> Bool
checkColors colorAmounts =
    let trimmed = trimLeft colorAmounts
        amountPart = (splitOn ' ' trimmed) !! 0
        color = (splitOn ' ' trimmed) !! 1
        amount = read amountPart :: Int
        reds = 12
        greens = 13
        blues = 14
    in case color of
        "red" -> amount <= reds
        "green" -> amount <= greens
        "blue" -> amount <= blues
        otherwise -> error ("unknown color: '" ++ color ++ "'")

validDraws :: String -> Bool
validDraws draws = all (== True) (map validDraw $ splitOn ';' draws)
    where validDraw draw = all (== True) (map checkColors $ splitOn ',' draw)

processLines :: [String] -> Int -> Int
processLines [] result = result
processLines (line:rest) result =
    let gamePart = (splitOn ':' line) !! 0
        drawsPart = (splitOn ':' line) !! 1
        gameIdPart = (splitOn ' ' gamePart) !! 1
        gameId = (read gameIdPart) :: Int
    in if (validDraws drawsPart)
        then processLines rest (result + gameId)
        else processLines rest result

process :: String -> Int
process contents = processLines (lines contents) 0

main :: IO ()
main =
    do
        args <- getArgs
        if (length args) < 1
            then usage
            else do
                     let filename = head args
                     contents <- readFile filename
                     let result = process contents
                     putStrLn ("result = " ++ (show result))
