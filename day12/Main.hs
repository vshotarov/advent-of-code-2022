module Main where

import qualified Common
import Data.Char (ord)
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day12 --"
    input <- Common.readInput

    -- Parse
    let (heightmap,start,end) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show heightmap)

    -- Solve
    let answer1 = bfs heightmap start (\(x,y,_) -> (x,y) == end) (\e e' -> e' - e <= 1)
    let answer2 = bfs heightmap end (\(_,_,e) -> e == 0) (\e e' -> e - e' <= 1)

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Heightmap = [[Int]]
type Point = (Int,Int)
type PointAndElevation = (Int,Int,Int)

parse :: String -> (Heightmap, Point, Point)
parse input =
    let remapOne 'S' = 0
        remapOne 'E' = 25
        remapOne  x  = ord x - 97
        heightmap = map (map remapOne) $ lines input
        points = concat
               . map (\(y,row) -> map (\(x,c) -> (x,y,c)) $ zip [0..] row)
               . zip [0..] $ lines input
        start = (\(x,y,_) -> (x,y)) . head $ filter (\(_,_,c) -> c == 'S') points
        end = (\(x,y,_) -> (x,y)) . head $ filter (\(_,_,c) -> c == 'E') points
     in (heightmap, start, end)

bfs :: Heightmap -> Point -> (PointAndElevation -> Bool) -> (Int -> Int -> Bool) -> Int
bfs heightmap (sx,sy) isEnd isAllowed = go [(sx,sy,0)] S.empty
    where go [] _ = error "could not find path"
          go ((x,y,s):toExplore) visited
            | S.member (x,y,s) visited = go toExplore visited
            | isEnd (x,y, (heightmap !! y) !! x) = s
            | otherwise = let neighbours = [(nx,ny) | (nx,ny) <- [(x+1,y),(x-1,y),(x,y+1),(x,y-1)],
                                                      nx >= 0 && ny >= 0
                                                   && (nx < length (head heightmap)) && ny < length heightmap]
                              neighbourElevations = map (\(nx,ny) -> (nx,ny,(heightmap !! ny) !! nx)) neighbours
                              elevation = (heightmap !! y) !! x
                              toExplore' = toExplore ++ map (\(nx,ny,_) -> (nx,ny,s+1))
                                                        (filter (\(_,_,e) -> isAllowed elevation e)
                                                                neighbourElevations)
                           in go toExplore' (S.insert (x,y,s) visited)
