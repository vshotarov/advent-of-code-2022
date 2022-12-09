module Main where

import qualified Common
import qualified Data.Set as S
import Data.Char (chr)

main :: IO ()
main = do
    putStrLn $ "-- Solving day09 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let states = map (\(_,h,t) -> (h,t))
               . takeWhile (\(is,_,_) -> length is > 0)
               $ iterate solve (parsedInput, (0,0), take 9 $ repeat (0,0))
    let answer1 = length . S.fromList $ map (head . snd) states
    let answer2 = length . S.fromList $ map (last . snd) states

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Point = (Int,Int)

parse :: String -> [(String,Int)]
parse input = map parseOne $ lines input
    where parseOne x = let (dir,steps) = Common.splitOnceOn " " x
                        in (dir,read steps)

solve :: ([(String,Int)],Point,[Point]) -> ([(String,Int)],Point,[Point])
solve s@([],_,_) = s
solve (((_,0):xs),h,t) = (xs,h,t)
solve (((dir,n):xs),h,t) = (((dir,n-1):xs),h',t')
    where (h',t') = step dir h t


step :: String -> Point -> [Point] -> (Point,[Point])
step dir h t = (h',t')
    where h' = processHead h dir
          buildTail cs | length cs < 2 = []
          buildTail (a:b:cs) = let processed = processTail a b
                                in processed:(buildTail (processed:cs))
          buildTail _ = error "unsupported tail length"
          t' = buildTail (h':t)

processHead :: Point -> String -> Point
processHead (x,y) "R" = (x+1,y)
processHead (x,y) "L" = (x-1,y)
processHead (x,y) "U" = (x,y+1)
processHead (x,y) "D" = (x,y-1)
processHead _ i = error ("unrecognised instruction " ++ i)

processTail :: Point -> Point -> Point
processTail (hx,hy) t@(tx,ty) = t'
    where (dx,dy) = (hx-tx,hy-ty)
          t' = case () of _
                            | dx /= 0 && dy /= 0 && abs dx + abs dy > 2 ->
                                (tx + sign dx, ty + sign dy)
                            | abs dx > 1 -> (tx + sign dx,ty)
                            | abs dy > 1 -> (tx,ty + sign dy)
                            | otherwise -> t

sign :: Int -> Int
sign x = if x >= 0 then 1 else -1

draw :: Point -> [Point] -> String
draw h t = concat $ map (\y -> '\n':(map (\x -> go (x,y)) [-15..15])) [15,14..(-15)]
    where go p
            | p == h = 'H'
            | p `elem` t = chr (49 + (Common.firstIdWhere (==p) t))
            | p == (0,0) = 's'
            | otherwise  = '.'
