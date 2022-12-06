module Main where

import qualified Common
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day06 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = solve 4 parsedInput
    let answer2 = solve 14 parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

solve :: Int -> String -> Int
solve n xs
    | length xs < n                          = error
                                                 "couldn't find start-of-message marker"
    | (length . S.fromList $ take n xs) == n = n
    | otherwise                              = 1 + (solve n $ tail xs)
