module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Set as S


main :: IO ()
main = do
    putStrLn $ "-- Solving day15 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = S.size $ getCoverageOnRow (if length parsedInput < 20 then 10 else 2000000)
                                             parsedInput
    let upperRule = if length parsedInput < 20 then 20 else 4000000

    let diamondPoints = foldr (++) [] $ map pointsAlongDiamond parsedInput
    let answer2 = 4000000 * px + py
            where isNotInAnyDiamond p = not $ any (isPointInDiamond p) parsedInput
                  go [] = error "couldn't solve"
                  go (p@(x,y):xs)
                    | x < 0 || y < 0 || x > upperRule || y > upperRule = go xs
                    | isNotInAnyDiamond p = p
                    | otherwise = go xs
                  (px,py) = go diamondPoints

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

parse :: String -> [[Int]]
parse input = map parseOne $ lines input
    where parseOne line = let p = concatMap (map (read . drop 2) . splitOn ", "
                                           . snd . Common.splitOnceOn " at ")
                                $ splitOn ": " line
                           in p

getCoverageOnRow :: Int -> [[Int]] -> S.Set ([Int])
getCoverageOnRow row sbs = S.filter (not . (`S.member` beacons))
                         $ foldr go S.empty sbs
    where beacons = S.fromList $ map (drop 2) sbs
          go [sx,sy,bx,by] coverage = let distToBeacon = abs (bx-sx) + abs (by-sy)
                                          diff = max 0 $ distToBeacon - abs (sy - row)
                                          covered = [[x,row] | x <- [(sx-diff)..(sx+diff)]]
                                       in foldr S.insert coverage covered
          go _ _ = error "invalid sensor beacon pair"

pointsAlongDiamond :: [Int] -> [(Int,Int)]
pointsAlongDiamond [sx,sy,bx,by] =
    let d = abs (bx-sx) + abs (by-sy) + 1
        upperLeftLine  = [(sx-d+i,sy+i) | i <- [0..(d-1)]]
        upperRightLine = [(sx+i,sy+d-i) | i <- [0..(d-1)]]
        lowerRightLine = [(sx+d-i,sy-i) | i <- [0..(d-1)]]
        lowerLeftLine  = [(sx-i,sy-d+i) | i <- [0..(d-1)]]
     in upperLeftLine ++ upperRightLine ++ lowerRightLine ++ lowerLeftLine
pointsAlongDiamond _ = error "invalid sensor beacon pair"

isPointInDiamond :: (Int,Int) -> [Int] -> Bool
isPointInDiamond (x,y) [sx,sy,bx,by] =
    let d = abs (bx-sx) + abs (by-sy)
        pd = abs (x-sx) + abs (y-sy)
     in pd <= d
isPointInDiamond _ _ = error "invalid sensor beacon pair"
