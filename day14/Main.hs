module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (sortOn)
import qualified Data.Set as S
import Data.Maybe (isNothing)

main :: IO ()
main = do
    putStrLn $ "-- Solving day14 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let grid = lineSegsToGrid $ concat parsedInput
    let states1 = takeWhile (not . isNothing) . iterate (moveSand (500,0)) $ Just grid
    let answer1 = length states1 - 1
    let filledState = fillSand floor grid
            where floor = 2 + (maximum . S.toList $ S.map snd grid)
    let answer2 = (S.size filledState) - (S.size grid)

    putStrLn $ drawGrid grid filledState

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Point = (Int,Int)
type LineSeg = (Point,Point)
type Grid = S.Set Point

moveSand :: Point -> Maybe Grid -> Maybe Grid
moveSand _ Nothing = Nothing
moveSand pt@(x,y) (Just grid) =
    let head' [] = Nothing
        head' (h:_) = Just h
        straightDownPt = head' . sortOn snd . S.toList $ S.filter (\(x',y') -> x' == x && y' > y) grid
        straightDownPt'= case straightDownPt of
                           Nothing      -> Nothing
                           Just (x',y') -> if y' == y+1 then Nothing else Just (x',y'-1)
        downLeftPt     = if (x-1,y+1) `S.member` grid then Nothing else Just (x-1,y+1)
        downRightPt    = if (x+1,y+1) `S.member` grid then Nothing else Just (x+1,y+1)
     in case (straightDownPt,straightDownPt',downLeftPt,downRightPt) of
            (Nothing,_,_,_)  -> Nothing
            (_,Just pt',_,_) -> moveSand pt' $ Just grid
            (_,_,Just pt',_) -> moveSand pt' $ Just grid
            (_,_,_,Just pt') -> moveSand pt' $ Just grid
            _                -> Just $ S.insert pt grid

parse :: String -> [[LineSeg]]
parse input = map parseOne $ lines input
    where parseOne line = let pts = map readPt $ splitOn " -> " line
                              readPt pt = let (x,y) = Common.splitOnceOn "," pt
                                           in (read x, read y) :: Point
                           in zip pts $ tail pts

lineSegsToGrid :: [LineSeg] -> Grid
lineSegsToGrid lineSegs = S.fromList $ foldr insertLineSeg [] lineSegs
    where insertLineSeg ((x1,y1),(x2,y2)) acc =
                        foldr (:) acc
                              [(x,y) | x <- [(min x1 x2)..(max x1 x2)],
                                       y <- [(min y1 y2)..(max y1 y2)]]

drawGrid :: Grid -> Grid -> String
drawGrid origGrid grid =
    let minX = (minimum . S.toList $ S.map fst grid) - 2
        maxX = (maximum . S.toList $ S.map fst grid) + 2
        minY = (minimum . S.toList $ S.map snd grid) - 2
        maxY = (maximum . S.toList $ S.map snd grid) + 2
     in concatMap (\y ->
                    "\n" ++ (map (\x ->
                                    if (x,y) `S.member` grid
                                        then (if (x,y) `S.member` origGrid then '#' else 'o')
                                        else '.')
                                 [minX..maxX]))
                  [minY..maxY]

fillSand :: Int -> Grid -> Grid
fillSand floor = go []
    where inPyramid (x,y) = x+y >= 500 && x-y <= 500 && y < floor
          go [] g
            | (500,0) `S.member` g = g
            | otherwise            = go [(500,0)] g
          go ((x,y):toExplore) g = case filter (\p -> (not $ p `S.member` g) && inPyramid p)
                                               [(x,y+1),(x-1,y+1),(x+1,y+1)] of
                                     [] -> go toExplore $ S.insert (x,y) g
                                     ns -> go (ns ++ toExplore) g
