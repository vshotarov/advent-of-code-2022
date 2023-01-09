module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (chr)

import Debug.Trace

main :: IO ()
main = do
    putStrLn $ "-- Solving day24 --"
    input <- Common.readInput

    -- Parse
    let (size@(width,height),weather) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show (size,weather))

    -- Solve
    let answer1 = search size [(width-1,height)] ((1,0),weather)
    let answer2 = search size [(width-1,height),(1,0),(width-1,height)] ((1,0),weather)


    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Point = (Int,Int)
type Velocity = (Int,Int)
type Size = (Int,Int)
type WeatherMap = ([Point],[Velocity])
type State = (Point,WeatherMap)

parse :: String -> (Size,WeatherMap)
parse input =
    (((length . head $ lines input) - 1, (length $ lines input) - 1),
     foldr parseLine ([],[]) . zip [1..] . tail . init $ lines input)
    where parseLine (y,line) acc = go 1 (tail $ init line) acc
              where go _ [] out = out
                    go x (c:cs) (points,velocities)
                      | c == '.' = go (x+1) cs (points,velocities)
                      | c == '^' = go (x+1) cs (((x,y):points),((0,-1):velocities))
                      | c == '>' = go (x+1) cs (((x,y):points),((1,0):velocities))
                      | c == 'v' = go (x+1) cs (((x,y):points),((0,1):velocities))
                      | c == '<' = go (x+1) cs (((x,y):points),((-1,0):velocities))
                      | otherwise = error ("unrecognised character " ++ [c])

stepWeather :: Size -> S.Set (Point,Velocity) -> S.Set (Point,Velocity)
stepWeather (width,height) wmap =
    S.foldr foldOne S.empty wmap
        where foldOne ((px,py),v@(vx,vy)) acc =
                let px' = if px + vx == 0 then width -1
                             else if px + vx == width then 1 else px + vx
                    py' = if py + vy == 0 then height -1
                             else if py + vy == height then 1 else py + vy
                 in S.insert ((px',py'),v) acc

search :: Size -> [Point] -> State -> Int
search size@(width,height) goals (me,(points,velocities)) =
    go 0 goals (S.fromList [me]) (S.fromList $ zip points velocities) 
    where go i [] _ _ = i
          go i (g:oals) positions wmap
            | g `S.member` positions = trace ("reached " ++ show g ++ " after " ++ show i ++ " steps")
                                     $ go i oals (S.fromList [g]) wmap
            | otherwise = 
                          let wmap' = stepWeather size wmap
                              getNs (x,y) = S.fromList
                                          $ filter (\(x',y') -> ((x',y') `elem` (g:oals))
                                                              ||(x'==1 && y'==0)
                                                              ||(x'>0 && x'<width
                                                              && y'>0 && y'<height))
                                                   [(x+1,y),(x-1,y),(x,y+1),(x,y-1),(x,y)]
                              windPositions = S.map fst wmap'
                              ns = S.foldr (\n acc -> S.union (getNs n) acc) S.empty positions
                           in go (i+1) (g:oals) (S.difference (S.union positions ns) windPositions) wmap'

draw :: Size -> S.Set Point -> WeatherMap -> String
draw (width,height) positions (points,velocities) =
    let wmap = foldr (\(p,(vx,vy)) acc ->
                        case M.lookup p acc of
                          Nothing -> M.insert p (vx,vy) acc
                          Just (x,y) -> M.insert p (abs x + abs y + abs vx + abs vy,0) acc)
                     M.empty $ zip points velocities
     in concat [([c | x <- [0..width],
                      let c = case M.lookup (x,y) wmap of
                                Nothing -> if (x,y) `S.member` positions
                                              then 'E'
                                              else if (x==0 || x==width
                                                    || y==0 || y==height)
                                                    then '#'
                                                    else '.'
                                Just (0,-1) -> '^'
                                Just (1,0)  -> '>'
                                Just (0,1)  -> 'v'
                                Just (-1,0) -> '<'
                                Just (n,_)  -> chr (n+48)]
                  ++ "\n") | y <- [0..height]]
