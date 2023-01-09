module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day25 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = showSnafu $ decToSnafu fuel
            where fuel = sum $ map snafuToDec parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1

type SNAFU = [Int]

showSnafu :: SNAFU -> String
showSnafu [] = []
showSnafu (x:xs) = (case x of
                      -2 -> '='
                      -1 -> '-'
                      _  -> head (show x)):(showSnafu xs)

parse :: String -> [SNAFU]
parse input = map parseOne $ lines input
    where parseOne [] = []
          parseOne (x:xs) = (case x of
                               '=' -> -2
                               '-' -> -1
                               _   -> read [x]):(parseOne xs)

snafuToDec :: SNAFU -> Int
snafuToDec snafu = go snafu
    where go [] = 0
          go whole@(x:xs) = (x * (5 ^ (length whole - 1))) + go xs

decToSnafu :: Int -> SNAFU
decToSnafu dec
  | dec <= 0 = []
  | otherwise = let remainder = ((dec + 2) `mod` 5) - 2
                    dec' = (dec + 2) `div` 5
                 in (decToSnafu dec') ++ [remainder]
