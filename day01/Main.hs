module Main where

import qualified Common
import Data.List (sort)

main :: IO ()
main = do
    putStrLn $ "-- Solving day01 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = maximum $ map sum parsedInput
    let answer2 = sum . take 3 . reverse . sort $ map sum parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [[Int]]
parse input = go [] $ lines input
    where go buffer [] = [buffer]
          go buffer ("":xs) = buffer:(go [] xs)
          go buffer (x:xs) = go ((read x):buffer) xs
