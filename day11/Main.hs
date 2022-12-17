module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.List (sort)

main :: IO ()
main = do
    putStrLn $ "-- Solving day11 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let playNRounds n worryFunc = take (n+1) $ iterate playRound parsedInput
            where playRound monkeys = foldl (\acc x -> playOneMonkey worryFunc x acc)
                                            monkeys [0..((length monkeys) - 1)]
    let monkeyBusiness = foldr1 (*) . take 2 . reverse . sort . map mnumActions
    let answer1 = monkeyBusiness . last $ playNRounds 20 (\x -> x `div` 3)
    let answer2 = monkeyBusiness . last $ playNRounds 10000 (\x -> x `mod` testsProduct)
            where testsProduct = foldr1 (*) $ map mtest parsedInput


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
    where parseMonkey monkey = let (_id:items:op:test:_succ:_fail:_) = monkey
                                   id' = read (drop 7 $ init _id)
                                   items' = map read . splitOn ", "
                                          . last $ splitOn ": " items
                                   op' = snd $ Common.splitOnceOn "= " op
                                   op'' = case splitOn " " op' of
                                            [_,"*","old"] -> (\x -> x * x)
                                            [_,"+",val] -> (\x -> x + (read val))
                                            [_,"*",val] -> (\x -> x * (read val))
                                            x -> error ("Unrecognised op: " ++ show x)
                                   test' = (read . last $ splitOn "by " test)
                                   _succ' = (read . last $ splitOn "monkey " _succ)
                                   _fail' = (read . last $ splitOn "monkey " _fail)
                                in Monkey id' items' op'' test' _succ' _fail' 0

playOneMonkey :: (Int -> Int) -> Int -> [Monkey] -> [Monkey]
playOneMonkey worryFunc i monkeys =
    let m = monkeys !! i
        items = mitems m
        items' = map (worryFunc . (mop m)) items
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
addItems (Monkey {mid=_id, mitems=items, mop=op, mtest=test,
                  msuccess=_succ, mfail=_fail, mnumActions=numActions}) newItems =
    Monkey _id (items ++ newItems) op test _succ _fail numActions

clearItems :: Monkey -> Monkey
clearItems (Monkey {mid=_id, mitems=items, mop=op, mtest=test,
                    msuccess=_succ, mfail=_fail, mnumActions=numActions}) =
    Monkey _id [] op test _succ _fail (numActions + length items)
