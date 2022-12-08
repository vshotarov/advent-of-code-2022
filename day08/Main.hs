module Main where

import qualified Common
import Data.List (transpose)
import Data.Char (ord)

main :: IO ()
main = do
    putStrLn $ "-- Solving day08 --"
    input <- Common.readInput

    -- Parse
    let (rows,cols) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show (rows,cols))

    -- Solve
    let answer1 = sum . map sum
                . map (map (fromEnum . uncurry (||)) . uncurry zip)
                $ zip visibleRows (transpose visibleCols)
            where visibleRows = map (markVisible (-1)) rows
                  visibleCols = map (markVisible (-1)) cols
    let answer2 = maximum . map maximum
                . map (\(row,col) -> map (\(a,b) -> a * b) $ zip row col)
                $ zip (tail . init $ scenicRows) (tail . init $ transpose scenicCols)
            where scenicRows = map (checkScenic []) rows
                  scenicCols = map (checkScenic []) cols

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> ([[Int]],[[Int]])
parse input = (asInts, transpose $ asInts) 
    where lines' = lines input
          asChar x = (ord x) - 48
          asInts = (map (map asChar) lines')

markVisible :: Int -> [Int] -> [Bool]
markVisible _ [] = []
markVisible m (x:xs)
    | null xs                 = True:(markVisible (max m x) xs)
    | x > m || x > maximum xs = True:(markVisible (max m x) xs)
    | otherwise               = False:(markVisible (max m x) xs)

checkScenic :: [Int] -> [Int] -> [Int]
checkScenic _ [] = []
checkScenic prev (x:xs)
    | null xs || null prev = 0:(checkScenic (x:prev) xs)
    | otherwise = (getVisible prev
                 * getVisible xs):(checkScenic (x:prev) xs)
        where getVisible [] = 0
              getVisible (y:ys)
                | y < x     = 1 + (getVisible ys)
                | otherwise = 1
