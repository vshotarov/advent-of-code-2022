module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day02 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum $ map (playRound . toShapes) parsedInput 
            where playRound (a,b) = 1 + fromEnum b + 3 * fromEnum (compare b a)
                  toShapes (a,b) = (toShape a,toShape b)
    let answer2 = sum $ map (playRound . toShapeAndOutcome) parsedInput 
            where playRound (a, Lose) =
                    0 + 1 + (fromEnum $ Common.firstWhere (<a) [Rock,Paper,Scissors])
                  playRound (a, Draw) = 3 + 1 + fromEnum a
                  playRound (a, Win) =
                    6 + 1 + (fromEnum $ Common.firstWhere (>a) [Rock,Paper,Scissors])
                  toShapeAndOutcome (a,b) = (toShape a,toOutcome b)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data Shape = Rock
           | Paper
           | Scissors
           deriving (Show,Eq,Enum)

instance (Ord Shape) where
    Rock <= Paper = True
    Rock <= Scissors = False
    Paper <= Scissors = True
    a <= b = b >= a

toShape :: String -> Shape
toShape x | x == "A" || x == "X" = Rock
toShape x | x == "B" || x == "Y" = Paper
toShape x | x == "C" || x == "Z" = Scissors
toShape x = error ("Unrecognised shape " ++ x)

data Outcome = Lose
             | Draw
             | Win
             deriving (Show)

toOutcome :: String -> Outcome
toOutcome "X" = Lose
toOutcome "Y" = Draw
toOutcome "Z" = Win
toOutcome x = error ("Unrecognised outcome " ++ x)

parse :: String -> [(String,String)]
parse input = map (Common.splitOnceOn " ") $ lines input
