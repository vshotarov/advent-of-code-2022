module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day04 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = length
                $ filter
                    (\((aLow,aHigh),(bLow,bHigh)) ->
                        (aLow >= bLow && aHigh <= bHigh)
                     || (bLow >= aLow && bHigh <= aHigh))
                parsedInput
    let answer2 = length
                $ filter
                    (\((aLow,aHigh),(bLow,bHigh)) ->
                        (aLow <= bHigh && aHigh >= bHigh)
                     || (bLow <= aHigh && bHigh >= aHigh))
                parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [((Int,Int),(Int,Int))]
parse input = map parseOne $ lines input
    where parseOne xs = let (a,b) = Common.splitOnceOn "," xs
                            (aLow,aHigh) = Common.splitOnceOn "-" a
                            (bLow,bHigh) = Common.splitOnceOn "-" b
                         in ((read aLow, read aHigh), (read bLow, read bHigh))
