module Main where

import qualified Common
import Data.List.Split (chunksOf)

main :: IO ()
main = do
    putStrLn $ "-- Solving day10 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let cycles = process parsedInput
    let answer1 = sum $ map (\c -> c * cycles !! (c-1)) [20,60..220]
    let answer2 = chunksOf 40
                . map draw
                $ zip [0..] cycles
                    where draw (c,x)
                            | abs (c `mod` 40 - x) < 2 = '#'
                            | otherwise                = '.'
    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn "Part 2: "
    mapM_ putStrLn answer2

parse :: String -> [String]
parse input = lines input

process :: [String] -> [Int]
process instructions = go 1 instructions
    where go _ [] = []
          go x ("noop":is) = x:(go x is)
          go x (i:is) | take 4 i == "addx" = x:x:(go (x+value) is)
              where value = (read $ drop 5 i) :: Int
          go _ (i:_) = error ("unrecognised instruction " ++ i)
