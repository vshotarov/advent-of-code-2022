module Main where

import qualified Common

main :: IO ()
main = do
    putStrLn $ "-- Solving day20 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let n = length parsedInput
    let getNs xs = let id0 = Common.firstIdWhere ((==0) . snd) xs
                    in sum [x | (i,(_,x)) <- zip [0..] xs,
                                i `elem` [((id0 + y) `mod` n) |
                                          y <- [1000,2000,3000]]]
    let answer1 = getNs $ bruteForce parsedInput parsedInput
    let answer2 = getNs . last . take 11 $ iterate (bruteForce ns) ns
            where ns = map (\(i,x) -> (i,x * 811589153)) parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2


parse :: String -> [(Int,Int)]
parse = zip [0..] . map read . lines

bruteForce :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
bruteForce order nums = go order nums
    where lenNums = length order
          go [] ys = ys
          go (x:xs) ys =
            go xs (insertAt i' x (popAt i ys))
                where i = Common.firstIdWhere (==x) ys
                      i' = (i + (snd x)) `mod` (lenNums-1)

popAt :: Int -> [a] -> [a]
popAt _ [] = error "can't pop nothing"
popAt i xs = (take i xs) ++ (drop (i+1) xs)

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = (take i xs) ++ [x] ++ (drop i xs)
