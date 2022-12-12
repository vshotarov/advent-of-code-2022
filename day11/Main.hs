module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (transpose)
import Data.List (sort)

import Debug.Trace

main :: IO ()
main = do
    putStrLn $ "-- Solving day11 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let after20Rounds = take 21
                      $ iterate playRound parsedInput
            where playRound monkeys = foldl (\acc x -> playOneMonkey x acc)
                                            monkeys [0..((length monkeys) - 1)]
    let testsProduct = foldr1 (*) $ map mtest parsedInput
    let after20Rounds' = take 10001
                      $ iterate playRound parsedInput
            where playRound monkeys = foldl (\acc x -> playOneMonkey' testsProduct x acc)
                                            monkeys [0..((length monkeys) - 1)]
    let answer1 = foldr1 (*) . take 2 . reverse . sort . map mnumActions
                $ last after20Rounds
    let answer2 = foldr1 (*) . take 2 . reverse . sort . map mnumActions
                $ last after20Rounds'

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data Monkey = Monkey {mid :: Int, mitems :: [Int], mop :: (Int -> Int),
                      mtest :: Int, msuccess :: Int, mfail :: Int, mnumActions :: Int}

instance Show Monkey where
    show m =
        "Monkey " ++ show (mid m,mitems m,mtest m,msuccess m,mfail m,mnumActions m)

parse :: String -> [Monkey]
parse input = map parseMonkey . splitOn [""] $ lines input
    where parseMonkey monkey = let [id,items,op,test,succ,fail] = monkey
                                   id' = read (drop 7 $ init id)
                                   items' = map read . splitOn ", "
                                          . last $ splitOn ": " items
                                   op' = snd $ Common.splitOnceOn "= " op
                                   op'' = case splitOn " " op' of
                                            [_,"+","old"] -> (\x -> x + x)
                                            [_,"*","old"] -> (\x -> x * x)
                                            [_,"+",val] -> (\x -> x + (read val))
                                            [_,"*",val] -> (\x -> x * (read val))
                                            x -> error ("Unrecognised op: " ++ show x)
                                   test' = (read . last $ splitOn "by " test)
                                   succ' = (read . last $ splitOn "monkey " succ)
                                   fail' = (read . last $ splitOn "monkey " fail)
                                in Monkey id' items' op'' test' succ' fail' 0

playOneMonkey :: Int -> [Monkey] -> [Monkey]
playOneMonkey i monkeys = let m = monkeys !! i
                              items = mitems m
                              items' = map ((`div` 3) . (mop m)) items
                              successes = filter ((==0) . (`mod` (mtest m))) items'
                              failures = filter (not . (`elem` successes)) items'
                              successM = monkeys !! (msuccess m)
                              failureM = monkeys !! (mfail m)
                              successM' = addItems successM successes
                              failureM' = addItems failureM failures
                              monkeys' = take (msuccess m) monkeys ++ (successM':(drop (msuccess m + 1) monkeys))
                              monkeys'' = take (mfail m) monkeys' ++ (failureM':(drop (mfail m + 1) monkeys'))
                              m' = clearItems m
                              monkeys''' = take i monkeys'' ++ (m':(drop (i+1) monkeys''))
                           in monkeys'''

playOneMonkey' :: Int -> Int -> [Monkey] -> [Monkey]
playOneMonkey' x i monkeys = let m = monkeys !! i
                                 items = mitems m
                                 items' = map ((`mod` x) . (mop m)) items
                                 successes = filter ((==0) . (`mod` (mtest m))) items'
                                 failures = filter (not . (`elem` successes)) items'
                                 successM = monkeys !! (msuccess m)
                                 failureM = monkeys !! (mfail m)
                                 successM' = addItems successM successes
                                 failureM' = addItems failureM failures
                                 monkeys' = take (msuccess m) monkeys ++ (successM':(drop (msuccess m + 1) monkeys))
                                 monkeys'' = take (mfail m) monkeys' ++ (failureM':(drop (mfail m + 1) monkeys'))
                                 m' = clearItems m
                                 monkeys''' = take i monkeys'' ++ (m':(drop (i+1) monkeys''))
                              in monkeys'''

addItems :: Monkey -> [Int] -> Monkey
addItems (Monkey {mid=id, mitems=items, mop=op, mtest=test,
                  msuccess=succ, mfail=fail, mnumActions=numActions}) newItems =
    Monkey id (items ++ newItems) op test succ fail numActions

clearItems :: Monkey -> Monkey
clearItems (Monkey {mid=id, mitems=items, mop=op, mtest=test,
                    msuccess=succ, mfail=fail, mnumActions=numActions}) =
    Monkey id [] op test succ fail (numActions + length items)
