module Main where

import qualified Common
import qualified Data.Map as M

main :: IO ()
main = do
    putStrLn $ "-- Solving day22 --"
    input <- Common.readInput

    -- Parse
    let (board,instructions) = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show (board,instructions))

    -- Solve
    let firstX = length $ takeWhile (==' ') $ board !! 0
    let path = walk board instructions [(firstX,0,(0,-1))]
    let answer1 = let (x,y,d) = head path
                   in 1000 * (y+1) + 4 * (x+1) + case d of
                                                   (1,0)  -> 0
                                                   (0,1)  -> 1
                                                   (-1,0) -> 2
                                                   (0,-1) -> 3
    let path2 = walk2 board instructions [(firstX,0,(0,-1))]
    let answer2 = let (x,y,d) = head path2
                   in 1000 * (y+1) + 4 * (x+1) + case d of
                                                   (1,0)  -> 0
                                                   (0,1)  -> 1
                                                   (-1,0) -> 2
                                                   (0,-1) -> 3

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

type Board = [String]
type Instruction = (Char,Int)
type State = (Int,Int,(Int,Int))
type Face = (Int,(Int,Int),(Int,Int))
type Cube = M.Map Int (M.Map Int (State -> State))

getFace :: Int -> (Int,Int) -> Int
getFace from (x,y)
  | from == 0 && x >=  50 && y >=   0 && x <  100 && y <  50 = 1
  | from == 0 && x >= 100 && y >=   0 && x <  150 && y <  50 = 2
  | from == 0 && x >=  50 && y >=  50 && x <  100 && y < 100 = 3
  | from == 0 && x >=   0 && y >= 100 && x <   50 && y < 150 = 4
  | from == 0 && x >=  50 && y >= 100 && x <  100 && y < 150 = 5
  | from == 0 && x >=   0 && y >= 150 && x <   50 && y < 200 = 6
  | from == 1 && x < 50 = 4
  | from == 1 && y < 0  = 6
  | from == 2 && y < 0  = 6
  | from == 2 && x > 149 = 5
  | from == 2 = 3
  | from == 3 && x < 50 = 4
  | from == 3 = 2
  | from == 4 && x < 0 = 1
  | from == 4 = 3
  | from == 5 && x > 99 = 2
  | from == 5 = 6
  | from == 6 && x < 0 = 1
  | from == 6 && x > 49 = 5
  | from == 6 = 2
  | otherwise = error ("no can do " ++ show (x,y))

localise :: State -> State
localise (xx,yy,dd) = (xx `mod` 50, yy `mod` 50, dd)

relative :: Int -> State -> State
relative f (x,y,d) =
    let 
     in case f of
          1 -> localise (x-50,y,d)
          2 -> localise (x-100,y,d)
          3 -> localise (x-50,y-50,d)
          4 -> localise (x,y-100,d)
          5 -> localise (x-50,y-100,d)
          6 -> localise (x,y-150,d)

global :: Int -> State -> State
global 1 (x,y,d) = (50+x,y,d)
global 2 (x,y,d) = (100+x,y,d)
global 3 (x,y,d) = (50+x,50+y,d)
global 4 (x,y,d) = (x,100+y,d)
global 5 (x,y,d) = (50+x,100+y,d)
global 6 (x,y,d) = (x,150+y,d)

cube :: Cube
cube = M.fromList
     [(1, M.fromList [(3, id),
                      (2, id),
                      (6, (\(x,y,d) -> (0,x,(1,0)))),
                      (4, (\(x,y,d) -> (0,-y-1,(1,0))))]),
      (2, M.fromList [(1, id),
                      (3, (\(x,y,d) -> (-1,x,(-1,0)))),
                      (5, (\(x,y,d) -> (-1,-y-1,(-1,0)))),
                      (6, (\(x,y,d) -> (x,-1,(0,-1))))]),
      (3, M.fromList [(1, id),
                      (5, id),
                      (2, (\(x,y,d) -> (y,-1,(0,-1)))),
                      (4, (\(x,y,d) -> (y,0,(0,1))))]),
      (4, M.fromList [(6, id),
                      (5, id),
                      (1, (\(x,y,d) -> (0,-y-1,(1,0)))),
                      (3, (\(x,y,d) -> (0,x,(1,0))))]),
      (5, M.fromList [(3, id),
                      (4, id),
                      (2, (\(x,y,d) -> (-1,-y-1,(-1,0)))),
                      (6, (\(x,y,d) -> (-1,x,(-1,0))))]),
      (6, M.fromList [(4, id),
                      (2, (\(x,y,d) -> (x,0,(0,1)))),
                      (1, (\(x,y,d) -> (y,0,(0,1)))),
                      (5, (\(x,y,d) -> (y,-1,(0,-1))))])]

parse :: String -> (Board,[Instruction])
parse input = let (board,(instructions:[])) = Common.splitOnceOn [""] $ lines input
                  go [] (d,s) = (d,read s):[]
                  go (x:xs) (d,s)
                      | x == 'R' || x == 'L' = (d,read s):(go xs (x,[]))
                      | otherwise = go xs (d,s ++ [x])
                  ins' = go instructions ('R',[])
               in (board,ins')

get :: Board -> (Int,Int) -> Char
get b (x,y)
  | x < 0 || y < 0 || y >= length b || x >= length (b !! y) = ' '
get b (x,y) = (b !! y) !! x

walk2 :: Board -> [Instruction] -> [State] -> [State]
walk2 _ [] s = s
walk2 b ((turn,steps):xs) ((s@(x,y,d)):ss) =
    walk2 b xs $ (go steps (x,y,rotate d turn)) ++ [s] ++ ss
    where maxX = maximum (map length b) - 1
          rotate (dx,dy) 'R' = (-dy,dx)
          rotate (dx,dy) 'L' = (dy,-dx)
          go 0 (xx,yy,d') = (xx,yy,d'):[]
          go n (xx,yy,d'@(dx,dy)) =
              let y' = yy + dy
                  x' = xx + dx
                  v = get b (x',y')
               in case v of
                    ' ' -> let fromF = getFace 0 (xx,yy)
                               toF = getFace fromF (x',y')
                               fTof = (cube M.! fromF) M.! toF
                               s'@(xx',yy',_) = (global toF
                                            (localise
                                           $ fTof (relative fromF (xx,yy,d'))))
                               v' = get b (xx',yy')
                            in if v' == '.' then ((go (n-1) s') ++ [(xx,yy,d')])
                                            else [(xx,yy,d')]
                    '.' -> (go (n-1) (x',y',d')) ++ [(xx,yy,d')]
                    '#' -> [(xx,yy,d')]

walk :: Board -> [Instruction] -> [State] -> [State]
walk _ [] s = s
walk b ((turn,steps):xs) ((s@(x,y,d)):ss) =
    walk b xs $ (go steps (x,y) (x,y) (rotate d turn)) ++ [s] ++ ss
    where maxX = maximum (map length b) - 1
          rotate (dx,dy) 'R' = (-dy,dx)
          rotate (dx,dy) 'L' = (dy,-dx)
          go 0 (safeX,safeY) (xx,yy) d' = (safeX,safeY,d'):[]
          go n (safeX,safeY) (xx,yy) d'@(dx,dy) =
              let 
                  y' = (yy + dy) `mod` (length b)
                  x' = (xx + dx) `mod` (maxX + 1)
                  v = get b (x',y')
               in case v of
                    ' ' -> go n (safeX,safeY) (x',y') d'
                    '.' -> (go (n-1) (x',y') (x',y') d') ++ [(safeX,safeY,d')]
                    '#' -> (safeX,safeY,d'):[]

draw :: [State] -> Board -> String
draw ss b =
    let maxX = maximum (map length b) - 1
        bAsMap = M.fromList
               $ [((x,y),get b (x,y)) | x <- [0..maxX],
                                        y <- [0..((length b)-1)]]
        bAsMap' = foldr insertOne bAsMap ss
        insertOne (x,y,(1,0)) = M.insert (x,y) '>'
        insertOne (x,y,(-1,0)) = M.insert (x,y) '<'
        insertOne (x,y,(0,1)) = M.insert (x,y) 'v'
        insertOne (x,y,(0,-1)) = M.insert (x,y) '^'
     in concat $ map (\y -> (map (\x -> bAsMap' M.! (x,y)) [0..maxX]) ++ "\n") [0..(length b - 1)]
