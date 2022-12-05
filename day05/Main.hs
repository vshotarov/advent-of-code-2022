module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (transpose)

main :: IO ()
main = do
    putStrLn $ "-- Solving day05 --"
    input <- Common.readInput

    -- Parse
    let (stacks,instructions) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show (stacks,instructions))

    -- Solve
    let answer1 = map head
                $ foldl (\acc x -> move CraneMover9000 x acc) stacks instructions
    let answer2 = map head
                $ foldl (\acc x -> move CraneMover9001 x acc) stacks instructions

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data CraneModel = CraneMover9000
                | CraneMover9001
                deriving (Show)
type Stacks = [String]
type Move = (Int,Int,Int)

parse :: String -> (Stacks,[Move])
parse input = (stacks',instructions')
    where (stacks,instructions) = Common.splitOnceOn [""] $ lines input
          stacks' = filter ((>0) . length)
                  . map (filter (not . (`elem` "[ ]")))
                  . transpose $ init stacks
          parse1Instruction (_:n:_:from:_:to:_) = (read n, read from - 1, read to - 1)
          parse1Instruction x = error ("Unrecognised instruction" ++ show (x))
          instructions' = map (parse1Instruction . splitOn " ") instructions

move :: CraneModel -> Move -> Stacks -> Stacks
move craneModel (n,fromI,toI) stacks = map move' $ zip [0..] stacks
    where from = stacks !! fromI
          to = stacks !! toI
          to' = case craneModel of
                  CraneMover9000 -> (reverse $ take n from) ++ to
                  _              -> (take n $ from) ++ to
          from' = drop n from
          move' (i,stack)
            | i == fromI = from'
            | i == toI   = to'
            | otherwise  = stack
