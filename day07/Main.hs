module Main where

import qualified Common
import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (sort)

main :: IO ()
main = do
    putStrLn $ "-- Solving day07 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve

    let sizes = M.mapWithKey (\name _ -> getSize name) parsedInput
            where getSize name = foldr ((+) . getOneSize name) 0 $ parsedInput M.! name
                  getOneSize p ("dir",name) = getSize $ if p == "/"
                                                           then p ++ name
                                                           else p ++ "/" ++ name
                  getOneSize _ (size,_)     = read size
    let answer1 = sum . M.elems . M.filter (<= 100000) $ sizes :: Int
    let answer2 = let totalSize = sizes M.! "root"
                      availSpace = 70000000 - totalSize
                      spaceToFreeUp = 30000000 - availSpace
                   in head . sort . filter (>= spaceToFreeUp) $ M.elems sizes

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> M.Map String [(String,String)]
parse input = foldr (\(k,v) acc -> M.insertWith (++) k v acc) M.empty
            . map (\(k,v) -> if take 2 k == "//" then (tail k, v)
                                                 else (k,v))
            . go "root" $ lines input
    where go _ [] = []
          go d (x:xs)
            | take 4 x == "$ cd" = case (x !! 5) of
                                     '/' -> go "root" xs
                                     '.' -> go ("root" ++ (concat
                                              . map ("/"++)
                                              . tail
                                              . init $ splitOn "/" d))
                                              xs
                                     _  -> go (d ++ "/" ++ (drop 5 x)) xs
            | head x == '$' = go d xs
            | otherwise = case Common.splitOnceOn " " x of
                            ("dir", dir) -> (d,[("dir", dir)]):(go d xs)
                            (fsize, fname) -> (d,[(fsize,fname)]):(go d xs)
