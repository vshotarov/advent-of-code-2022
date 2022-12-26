module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Set as S
import Data.List (sortOn)

main :: IO ()
main = do
    putStrLn $ "-- Solving day19 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum
                $ map (\(bp@(i,_,_,_,_)) -> i * getMaxGeodesForBP 24 bp) parsedInput
    let answer2 = product
                $ map (\(bp@(i,_,_,_,_)) -> getMaxGeodesForBP 32 bp) $ take 3 parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Blueprint = (Int,Int,Int,(Int,Int),(Int,Int))
type Storage = (Int,Int,Int,Int)
type Robots = (Int,Int,Int,Int)

parse :: String -> [Blueprint]
parse input = map parseBlueprint $ lines input
    where parseBlueprint xs =
            let (bp,rest) = (\(a,b) -> (a, splitOn "." b)) (Common.splitOnceOn ":" xs)
                bpId = snd (Common.splitOnceOn " " bp)
                (_:_:_:_:_:ore:_) = splitOn " " $ (rest !! 0)
                (_:_:_:_:_:clay:_) = splitOn " " $ (rest !! 1)
                (_:_:_:_:_:obsOre:_:_:obsClay:_) = splitOn " " $ (rest !! 2)
                (_:_:_:_:_:geoOre:_:_:geoObs:_) = splitOn " " $ (rest !! 3)
             in (read bpId,read ore,read clay,
                 (read obsOre,read obsClay),(read geoOre, read geoObs))

getMaxGeodesForBP :: Int -> Blueprint -> Int
getMaxGeodesForBP mins (_id,ore,clay,(obsOre,obsClay),(geoOre,geoObs)) =
    go [(((0,0,0,0),(1,0,0,0)),mins)] S.empty 0
    where go [] _ best = best
          go (s@((ss@(ore,clay,obs,geo),rs@(oreRs,clayRs,obsRs,geoRs)),mins):toExplore) visited best
            | s `S.member` visited = go toExplore visited best
            | geo + (geoRs * mins + (sum [0..mins])) < best = go toExplore visited best
            | mins == 0 = go toExplore visited (max best geo)
            | otherwise = let as = [(ss,rs)]
                              as' = (if mins > 1 then [buyGeoRobot ss rs] else []) ++ as
                              as'' = (if mins > 2 then [buyObsRobot ss rs] else []) ++ as'
                              as''' = (if mins > 3 then [buyClayRobot ss rs, buyOreRobot ss rs]
                                                   else []) ++ as''
                              ns = filter (\((o,c,ob,g),_) -> all (>=0) [o,c,ob,g])
                                 $ sortOn (\(_,(_,_,_,grs)) -> grs) as'''
                              ns' = map (\(ss',rs') ->
                                         ((gatherResources ss' rs,rs'),mins-1)) ns
                           in go (ns' ++ toExplore) (S.insert s visited) best
          buyOreRobot s'@(o,c,ob,g) r'@(or,cr,obr,gr)
            | o - or > ore = (s',r')
            | obr > geoObs && or > geoOre = (s',r')
            | otherwise = ((o-ore,c,ob,g), (or+1,cr,obr,gr))
          buyClayRobot s'@(o,c,ob,g) r'@(or,cr,obr,gr)
            | o - or > clay = (s',r')
            | obr > geoObs = (s',r')
            | otherwise = ((o-clay,c,ob,g), (or,cr+1,obr,gr))
          buyObsRobot s'@(o,c,ob,g) r'@(or,cr,obr,gr)
            | obsOre - or > obsOre && c - cr > obsClay = (s',r')
            | obr > geoObs && or > geoOre = (s',r')
            | otherwise = ((o-obsOre,c-obsClay,ob,g), (or,cr,obr+1,gr))
          buyGeoRobot (o,c,ob,g) (or,cr,obr,gr)  = ((o-geoOre,c,ob-geoObs,g), (or,cr,obr,gr+1))
          gatherResources (o,c,ob,g) (or,cr,obr,gr) = (o+or,c+cr,ob+obr,g+gr)
