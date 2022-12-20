module Main where

import qualified Common
import Data.List (sortOn,sort)
import qualified Data.Set as S

import Debug.Trace

main :: IO ()
main = do
    putStrLn $ "-- Solving day17 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let solve = solve1 2022 parsedInput

    putStrLn "Part 2 is solved in day17/PART_2_ON_PAPER.txt"

    putStrLn $ show solve
    
data Shape = HLine (Int,Int)
           | Cross (Int,Int)
           | Corner (Int,Int)
           | VLine (Int,Int)
           | Square (Int,Int)
           deriving (Show)

parse input = input

solve1 :: Int -> String -> Int
solve1 numRocks air = go air (HLine (2, 3)) [] S.empty
    where go air' shape obstacles visited
            | length air' == 0 = go air shape obstacles visited
            | otherwise =
                let shape' = hMove (head air') shape
                    isShapeAtRest = doesIntersectShapes shape' obstacles
                    shape'' = vMove (if isShapeAtRest then shape else shape')
                    isShape'AtRest = doesIntersectShapes shape'' obstacles || isAt0 shape''
                    shapeObstacles = shape:obstacles
                    shape'Obstacles = shape':obstacles
                    visited' = S.insert (getShapeType shape,air') visited
                    result = case (isShapeAtRest,isShape'AtRest) of
                      (True,True) -> go (drop 1 air') (getNewShape shape shapeObstacles) shapeObstacles visited'
                      (True,False) -> go (drop 1 air') shape'' obstacles visited'
                      (_,True) -> go (drop 1 air') (getNewShape shape' shape'Obstacles) shape'Obstacles visited'
                      _        -> go (drop 1 air') shape'' obstacles visited'
                    result' = if isHLine shape && (getShapeType shape,air') `S.member` visited
                                 then trace ("visited " ++ show (getShapeType shape,air',getHeight obstacles,length obstacles)) result
                                 else result
                 in if length obstacles == numRocks
                       then trace ("Part 1: " ++ show (getHeight obstacles + 1)) $ result'
                       else result'

getNewShape :: Shape -> [Shape] -> Shape
getNewShape (HLine (_,y))  obstacles = Cross (2,(getHeight obstacles)+6)
getNewShape (Cross (_,y))  obstacles = Corner (2,(getHeight obstacles)+6)
getNewShape (Corner (_,y)) obstacles = VLine (2,(getHeight obstacles)+7)
getNewShape (VLine (_,y))  obstacles = Square (2,(getHeight obstacles)+5)
getNewShape (Square (_,y)) obstacles = HLine (2,(getHeight obstacles)+4)

doesIntersectShapes :: Shape -> [Shape] -> Bool
doesIntersectShapes shape obstacles = any (do2ShapesIntersect shape) obstacles

do2ShapesIntersect :: Shape -> Shape -> Bool
do2ShapesIntersect a b =
    (S.size $ S.intersection (shapeToCells a) (shapeToCells b)) > 0

shapeToCells :: Shape -> S.Set (Int,Int)
shapeToCells (HLine (x,y))  = S.fromList [(x,y),(x+1,y),(x+2,y),(x+3,y)]
shapeToCells (Cross (x,y))  = S.fromList [(x+1,y),(x+1,y-1),(x+1,y-2),(x,y-1),(x+2,y-1)]
shapeToCells (Corner (x,y)) = S.fromList [(x+2,y),(x+2,y-1),(x+2,y-2),(x+1,y-2),(x,y-2)]
shapeToCells (VLine (x,y))  = S.fromList [(x,y),(x,y-1),(x,y-2),(x,y-3)]
shapeToCells (Square (x,y)) = S.fromList [(x,y),(x+1,y),(x+1,y-1),(x,y-1)]

isAt0 :: Shape -> Bool
isAt0 (HLine (x,y)) = y < 0
isAt0 (Cross (x,y)) = y < 2
isAt0 (Corner (x,y)) = y < 2
isAt0 (VLine (x,y)) = y < 3
isAt0 (Square (x,y)) = y < 1

getHeight :: [Shape] -> Int
getHeight shapes = last . sort $ map getShapeY shapes

hMove :: Char -> Shape -> Shape
hMove '>' s@(HLine (x,y))
    | x > 2 = s
    | otherwise = HLine (x+1,y)
hMove '>' s@(Cross (x,y))
    | x > 3 = s
    | otherwise = Cross (x+1,y)
hMove '>' s@(Corner (x,y))
    | x > 3 = s
    | otherwise = Corner (x+1,y)
hMove '>' s@(VLine (x,y))
    | x > 5 = s
    | otherwise = VLine (x+1,y)
hMove '>' s@(Square (x,y))
    | x > 4 = s
    | otherwise = Square (x+1,y)
--- left
hMove '<' shape
    | getShapeX shape == 0 = shape
    | otherwise = hMove' shape
    where hMove' (HLine (x,y)) = HLine (x-1,y)
          hMove' (Cross (x,y)) = Cross (x-1,y)
          hMove' (Corner (x,y)) = Corner (x-1,y)
          hMove' (VLine (x,y)) = VLine (x-1,y)
          hMove' (Square (x,y)) = Square (x-1,y)

getShapeX :: Shape -> Int
getShapeX (HLine (x,_)) = x
getShapeX (Cross (x,_)) = x
getShapeX (Corner (x,_)) = x
getShapeX (VLine (x,_)) = x
getShapeX (Square (x,_)) = x

getShapeY :: Shape -> Int
getShapeY (HLine (_,y)) = y
getShapeY (Cross (_,y)) = y
getShapeY (Corner (_,y)) = y
getShapeY (VLine (_,y)) = y
getShapeY (Square (_,y)) = y

getShapeType :: Shape -> Int
getShapeType (HLine _) = 0
getShapeType (Cross _) = 1
getShapeType (Corner _) = 2
getShapeType (VLine _) = 3
getShapeType (Square _) = 4

vMove :: Shape -> Shape
vMove (HLine (x,y)) = HLine (x,y-1)
vMove (Cross (x,y)) = Cross (x,y-1)
vMove (Corner (x,y)) = Corner (x,y-1)
vMove (VLine (x,y)) = VLine (x,y-1)
vMove (Square (x,y)) = Square (x,y-1)

isHLine :: Shape -> Bool
isHLine (HLine _) = True
isHLine _ = False

draw :: Shape -> [Shape] -> String
draw shape shapes = concat
            $ map (\y -> "\n" ++ map (\x -> if (x,y) `S.member` cells'
                                               then '@' else (if (x,y) `S.member` cells then '#' else '.')) [0..6])
                  [maxY,maxY-1..0]
    where cells = foldr (S.union . shapeToCells) S.empty shapes
          cells' = shapeToCells shape
          maxY = snd . last . sortOn snd $ S.toList $ S.union cells' cells
