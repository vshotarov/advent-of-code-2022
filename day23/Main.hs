module Main where

import qualified Common
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust,isNothing)

main :: IO ()
main = do
    putStrLn $ "-- Solving day23 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = foldr (\x -> if x == '.' then (+1) else id) 0
                . draw . fst . fromJust . last . take 11 . iterate (step . fromJust)
                $ Just (parsedInput,"NSWE")
    let answer2 = length . takeWhile (not . isNothing) . iterate (step . fromJust)
                $ Just (parsedInput,"NSWE")

    -- Print answers
    putStrLn $ "Part 1: " ++ show (answer1 :: Int)
    putStrLn $ "Part 2: " ++ show answer2

type Point = (Int,Int)
type Grid = S.Set Point
type DirectionOrder = String
type State = (Grid,DirectionOrder)

parse :: DirectionOrder -> Grid
parse input = foldr parseLine S.empty . zip [0..] $ lines input
    where parseLine (y,line) grid = go 0 line grid
              where go _ [] acc = acc
                    go i (x:xs) acc
                      | x == '#'  = go (i+1) xs $ S.insert (i,y) acc
                      | otherwise = go (i+1) xs acc

dirToVec :: Char -> (Int,Int)
dirToVec 'N' = (0,-1)
dirToVec 'S' = (0,1)
dirToVec 'W' = (-1,0)
dirToVec 'E' = (1,0)
dirToVec _ = error "unrecognised direction"

getProposedPosition :: DirectionOrder -> Point -> Grid -> Maybe Point
getProposedPosition order (x,y) grid =
    let getAdj dir = case dir of
                       'N' -> [(x-1,y-1),(x,y-1),(x+1,y-1)] 
                       'S' -> [(x-1,y+1),(x,y+1),(x+1,y+1)] 
                       'W' -> [(x-1,y-1),(x-1,y),(x-1,y+1)] 
                       'E' -> [(x+1,y-1),(x+1,y),(x+1,y+1)] 
                       _   -> error "unrecognised direction"
        adj = [ps | ps <- map getAdj order,
                    all (not . (`S.member` grid)) ps]
     in case adj of
          [] -> Nothing
          [_,_,_,_] -> Nothing
          (([_,(x',y'),_]):_) -> Just (x',y')
          a -> error ("unreachable" ++ show a)

step :: State -> Maybe State
step (grid,order) =
    if grid == grid'
       then Nothing
       else Just (grid',(tail order) ++ [head order])
    where go [] new _ = new
          go (p:ps) new moves =
              case getProposedPosition order p grid of
                Nothing -> go ps (S.insert p new ) (M.insert p p moves)
                Just p' -> case M.lookup p' moves of
                             Just oldP -> go ps (S.insert p
                                                    (S.insert oldP (S.delete p' new)))
                                                moves
                             Nothing   -> go ps (S.insert p' new) (M.insert p' p moves)
          grid' = go (S.toList grid) S.empty M.empty

draw :: Grid -> String
draw grid =
    let minX = minimum $ S.map fst grid
        minY = minimum $ S.map snd grid
        maxX = maximum $ S.map fst grid
        maxY = maximum $ S.map snd grid
     in concat [([c | x <- [minX..maxX],
                      let c = if S.member (x,y) grid
                                 then '#'
                                 else '.'] ++ "\n") | y <- [minY..maxY]]
