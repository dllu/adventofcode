module Main ( main ) where

import Data.Char
import Data.List
import Data.Maybe
import System.Environment
import System.Exit

usage =
    do
        progName <- getProgName    
        putStrLn ("usage: " ++ progName ++ " <file>")
        exitFailure

getDigit digit =
    if all isDigit digit
        then digit
        else case digit of
            "zero" -> "0"
            "one" -> "1"
            "two" -> "2"
            "three" -> "3"
            "four" -> "4"
            "five" -> "5"
            "six" -> "6"
            "seven" -> "7"
            "eight" -> "8"
            "nine" -> "9"
            _ -> error ("unexpected digit: " ++ (show digit))

findNumber [] line (minIndex, minDigit) (maxIndex, maxDigit) =
    read ((fromJust minDigit) ++ (fromJust maxDigit)) :: Int
findNumber (digit:digits) line (minIndex, minDigit) (maxIndex, maxDigit) =
    let indices = findIndices (isPrefixOf digit) (tails line)
        firstIndex = if not (null indices)
                         then Just (head indices)
                         else Nothing
        lastIndex = if not (null indices)
                        then Just (last indices)
                        else Nothing
        minPair = if (isJust firstIndex) && (fromJust firstIndex) <= minIndex
                      then (fromJust firstIndex, Just (getDigit digit))
                      else (minIndex, minDigit)
        maxPair = if (isJust lastIndex) && (fromJust lastIndex) >= maxIndex
                      then (fromJust lastIndex, Just (getDigit digit))
                      else (maxIndex, maxDigit)
    in findNumber digits line minPair maxPair

processLines [] nums = nums
processLines (line:lines) nums =
    let digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                  "zero", "one", "two", "three", "four", "five", "six", 
                  "seven", "eight", "nine"]
        number = findNumber digits line ((length line), Nothing) (0, Nothing)
    in processLines lines (nums ++ [number])

process contents =
    let nums = processLines (lines contents) [] 
    in sum nums

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
