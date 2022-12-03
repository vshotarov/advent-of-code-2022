module Main where

import qualified Common
import qualified Data.Set as S
import Data.List.Split (chunksOf)
import Data.Char (ord)

main :: IO ()
main = do
    putStrLn $ "-- Solving day03 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum
                $ map (\line -> toPriority
                              . S.elemAt 0
                              . foldr1 S.intersection
                              . map S.fromList
                              $ chunksOf (length line `div` 2) line)
                  parsedInput

    let answer2 = sum
                . map (toPriority . S.elemAt 0 . foldr1 S.intersection . map S.fromList)
                $ chunksOf 3 parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [String]
parse input = lines input

toPriority :: Char -> Int
toPriority x = if ordX >= 97
                  then ordX - 96
                  else 27 + ordX - 65
    where ordX = ord x
