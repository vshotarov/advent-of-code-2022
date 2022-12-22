module Main where

import qualified Common
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (sort, sortOn)

main :: IO ()
main = do
    putStrLn $ "-- Solving day16 --"
    input <- Common.readInput

    -- Parse
    let (rates,graph) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show (rates,graph))

    -- Solve
    let distances = buildDistanceCache graph
    let answer1 = dfs 30 rates distances

    let valves = filter (/= "AA") . M.keys $ M.filter (>0) rates
    let valveSet = S.insert "AA" $ S.fromList valves
    let valveSubsets = map (S.fromList . ("AA":)) . filter ((>0) . length)
                     $ subsets valves
    let paths = M.fromList
              $ map (\x -> (x,dfs 26 (M.filterWithKey (\k _ -> k `S.member` x) rates)
                                     distances))
                valveSubsets
    let answer2 = fst ((M.foldrWithKey processOne (0,0) paths) :: (Int,Int))
            where processOne vs _ (acc,c) | vs == valveSet = (acc,c+1)
                  processOne vs s (acc,c) = (max acc $ s + get vs' paths,c+1)
                         where vs' = S.insert "AA" $ S.difference valveSet vs

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Graph = M.Map String [String]
type DistanceCache = M.Map (String,String) Int

parse :: String -> (M.Map String Int, Graph)
parse input = foldr parseOne (M.empty,M.empty) $ lines input
    where parseOne x (rates,connections) =
            let (_:name:_:_:rate:_:_:_:_:valves) = splitOn " " x
                rate' = drop 5 $ init rate
                valves' = (last valves):(map init $ init valves)
             in (M.insert name (read rate') rates, M.insert name valves' connections)


getDistance :: String -> String -> Graph -> Int
getDistance from to graph = bfs [(from,0)] S.empty
  where bfs [] _ = error "no path"
        bfs ((x,s):xs) visited
          | x == to = s
          | (x,s) `S.member` visited = bfs xs visited
          | otherwise = let ns = get x graph
                         in bfs (xs ++ [(n,s+1) | n <- ns])
                                (S.insert (x,s) visited)

buildDistanceCache :: Graph -> DistanceCache
buildDistanceCache graph = go (M.keys graph) M.empty
    where go [] cache = cache
          go (v:alves) cache =
              go alves (foldr
                            (\v' -> let d = getDistance v v' graph
                                     in M.insert (v',v) d
                                      . M.insert (v,v') d)
                            cache alves)

dfs :: Int -> M.Map String Int -> DistanceCache -> Int
dfs steps rates cache =
    let maxRate   = sum . take 10 . reverse . sort $ M.elems rates
        go [] _ best = best
        go ((x,t,s,vs):xs) visited best
          | (x,t,s,vs) `S.member` visited = go xs visited best
          | s + t * maxRate < best = go xs visited best
          | t == 0 && s > best = go xs visited s
          | t == 0 = go xs visited $ max best s
          | otherwise = let ns = filter (\n -> n /= x
                                            && (get (x,n) cache) < (t-1)
                                            && not (n `S.member` vs))
                               $ M.keys $ M.filter (>0) rates
                            ds = map (\n -> get (x,n) cache) ns
                            rate = foldr (\x' acc -> acc + (get x' rates)) 0 vs
                            rate' = rate + (rates M.! x)
                            ns' = map (\(n,d) -> (n,t-1-d,s+rate+d*rate',S.insert x vs))
                                $ zip ns ds
                            end = [(x,0,s+rate+rate'*(t-1),vs)]
                         in go (sortOn (\(_,_,s',_) -> -s')
                                       (end ++ ns' ++ xs))
                               (S.insert (x,t,s,vs) visited) best
     in go [("AA",(steps+1),0,S.fromList ["AA"])] S.empty 0
        
get :: Show a => Show b => Ord a => a -> M.Map a b -> b
get k m = case M.lookup k m of
            Nothing -> error ("Looking for " ++ show k ++ " in " ++ show m)
            Just x -> x

subsets :: [a] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)
