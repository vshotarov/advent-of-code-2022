module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Map as M
import qualified Data.Set as S

main :: IO ()
main = do
    putStrLn $ "-- Solving day18 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = solve1 parsedInput
    let answer2 = answer1 - (solve1 (S.toList $ flood parsedInput))

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Point3 = (Int,Int,Int)

parse :: String -> [Point3]
parse input = map parseOne $ lines input
    where parseOne line = case map read $ splitOn "," line of
                            [x,y,z] -> (x,y,z)
                            _ -> error "invalid input"

solve1 :: [Point3] -> Int
solve1 cubes = 
    let grpdAlongX = foldr (\(x,y,z) -> M.insertWith (++) (y,z) [x])
                           M.empty cubes
        grpdAlongY = foldr (\(x,y,z) -> M.insertWith (++) (x,z) [y])
                           M.empty cubes
        grpdAlongZ = foldr (\(x,y,z) -> M.insertWith (++) (x,y) [z])
                           M.empty cubes
        findNumOverlaps as | length as < 2 = 0
        findNumOverlaps (a:b:as)
          | a + 1 == b = 1 + findNumOverlaps (b:as)
          | otherwise  = findNumOverlaps (b:as)
        findNumOverlaps _ = error "invalid input"
     in (length cubes * 6)
      - 2 * (sum $ map (sum . map (findNumOverlaps . sort) . M.elems)
         [grpdAlongX,grpdAlongY,grpdAlongZ])

getBBox :: [Point3] -> (Point3,Point3)
getBBox cubes = ((minX,minY,minZ),(maxX,maxY,maxZ))
    where xs = map (\(x,_,_) -> x) cubes
          ys = map (\(_,y,_) -> y) cubes
          zs = map (\(_,_,z) -> z) cubes
          (minX,maxX) = (minimum xs, maximum xs)
          (minY,maxY) = (minimum ys, maximum ys)
          (minZ,maxZ) = (minimum zs, maximum zs)

flood :: [Point3] -> S.Set Point3
flood cubes = 
    let ((minX,minY,minZ),(maxX,maxY,maxZ)) = getBBox cubes
        go [] visited = visited
        go ((x,y,z):toExplore) visited
          | (x,y,z) `S.member` visited = go toExplore visited
          | x < minX-1 || x > maxX+1   = go toExplore visited
          | y < minY-1 || y > maxY+1   = go toExplore visited
          | z < minZ-1 || z > maxZ+1   = go toExplore visited
          | otherwise = let ns = [(x+1,y,z),(x-1,y,z),
                                  (x,y+1,z),(x,y-1,z),
                                  (x,y,z+1),(x,y,z-1)]
                            ns' = filter (not . (`elem` cubes)) ns
                         in go (toExplore ++ ns') (S.insert (x,y,z) visited)
        filled = go [(minX-1,minY-1,minZ-1)] S.empty
        _all = S.fromList [(x,y,z) | x <- [(minX-1)..(maxX+1)],
                                     y <- [(minY-1)..(maxY+1)],
                                     z <- [(minZ-1)..(maxZ+1)]]
     in S.difference _all (S.union (S.fromList cubes) filled)


