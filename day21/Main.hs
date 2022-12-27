module Main where

import qualified Common
import Data.List.Split (splitOn)
import qualified Data.Map as M

import Debug.Trace

main :: IO ()
main = do
    putStrLn $ "-- Solving day21 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = process "root" parsedInput
    let [rootDepA,rootDepB] = getDirectDependencies (parsedInput M.! "root")
    let aDependencies = getDependencies parsedInput $ parsedInput M.! rootDepA
    let bDependencies = getDependencies parsedInput $ parsedInput M.! rootDepB
    let notHumanDep = fst . head
                    $ filter (not . ("humn" `elem`) . snd) [(rootDepA,aDependencies),
                                                            (rootDepB,bDependencies)]
    let humanDep = fst . head
                 $ filter (("humn" `elem`) . snd) [(rootDepA,aDependencies),
                                                   (rootDepB,bDependencies)]
    let processed = M.delete "root" $ processAllExceptHumn parsedInput
    let notHumanVal = getVal $ processed M.! notHumanDep
    let answer2 = expand humanDep notHumanVal processed

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data MonkeyOp = Yell Int
              | MonkeyOp String String (Int -> Int -> Int)

instance Show MonkeyOp where
    show (Yell x) = "Yell " ++ show x
    show (MonkeyOp a b f) = "MonkeyOp " ++ a ++ " " ++ b ++ " " ++ (showOp f)

showOp :: (Int -> Int -> Int) -> String
showOp f = case f 20 100 of
             -80 -> "-"
             120 -> "+"
             2000 -> "*"
             0 -> "/"

parse :: String -> M.Map String MonkeyOp
parse input = M.fromList . map parseOne $ lines input
    where parseOne x = let (n,op) = Common.splitOnceOn ": " x
                           op' = case splitOn " " op of
                                   [x] -> Yell (read x)
                                   [a,"+",b] -> MonkeyOp a b (+)
                                   [a,"-",b] -> MonkeyOp a b (-)
                                   [a,"/",b] -> MonkeyOp a b (div)
                                   [a,"*",b] -> MonkeyOp a b (*)
                         in (n,op')

process :: String -> M.Map String MonkeyOp -> Int
process name monkeys = go [name] monkeys
    where go [] state = case state M.! name of
                          Yell x -> x
                          _ -> error "nah"
          go (x:xs) state =
              case state M.! x of
                Yell x' -> go xs state
                MonkeyOp a b op -> let a' = state M.! a
                                       b' = state M.! b
                                    in case (a',b') of
                                         (MonkeyOp _ _ _,_) -> go (a:x:xs) state
                                         (_,MonkeyOp _ _ _) -> go (b:x:xs) state
                                         (Yell a'', Yell b'') -> go xs
                                                               $ M.insert x (Yell $ op a'' b'') state

getDirectDependencies :: MonkeyOp -> [String]
getDirectDependencies (Yell _) = []
getDirectDependencies (MonkeyOp a b _) = [a,b]

getDependencies :: M.Map String MonkeyOp -> MonkeyOp -> [String]
getDependencies mMap (Yell _) = []
getDependencies mMap (MonkeyOp a b _) =
    [a,b] ++ (getDependencies mMap $ mMap M.! a)
          ++ (getDependencies mMap $ mMap M.! b)

processAllExceptHumn :: M.Map String MonkeyOp -> M.Map String MonkeyOp
processAllExceptHumn ms = go (M.keys ms) ms
    where go [] state = state
          go (x:xs) state =
              case state M.! x of
                Yell x' -> go xs state
                mop@(MonkeyOp a b op) ->
                    let a' = state M.! a
                        b' = state M.! b
                        humnInA' = "humn" `elem` (getDependencies ms a')
                        humnInB' = "humn" `elem` (getDependencies ms b')
                     in case (a',b',humnInA',humnInB',"humn" `elem` [a,b]) of
                          (_,_,_,_,True) -> go xs state
                          (MonkeyOp _ _ _,_,False,_,_) -> go (a:x:xs) state
                          (_,MonkeyOp _ _ _,_,False,_) -> go (b:x:xs) state
                          (Yell a'', Yell b'',_,_,_) -> go xs
                                                    $ M.insert x (Yell $ op a'' b'') state
                          _ -> go xs state

getVal :: MonkeyOp -> Int
getVal (Yell a) = a

expand :: String -> Int -> M.Map String MonkeyOp -> Int
expand startAt value monkeys = go (monkeys M.! startAt) value
    where go (MonkeyOp a b op) v =
              let a' = monkeys M.! a
                  b' = monkeys M.! b
               in
                  if "humn" `elem` [a,b]
                     then case (a,b,showOp op) of
                            ("humn",_,"+") -> v - getVal b'
                            ("humn",_,"-") -> v + getVal b'
                            ("humn",_,"*") -> v `div` getVal b'
                            ("humn",_,"/") -> v * getVal b'
                     else case (a',b',showOp op) of
                    (Yell a'',_,"+") -> go b' (v-a'')
                    (Yell a'',_,"-") -> go b' (a''-v)
                    (Yell a'',_,"*") -> go b' (v `div` a'')
                    (Yell a'',_,"/") -> go b' (a'' `div` v)
                    (_,Yell b'',"+") -> go a' (v-b'')
                    (_,Yell b'',"-") -> go a' (b''+v)
                    (_,Yell b'',"*") -> go a' (v `div` b'')
                    (_,Yell b'',"/") -> go a' (b''*v)
