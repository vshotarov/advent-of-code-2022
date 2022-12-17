module Main where

import qualified Common
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.List (sortBy)

import Debug.Trace

main :: IO ()
main = do
    putStrLn $ "-- Solving day13 --"
    input <- Common.readInput

    -- Parse
    let parsedInput = parse input
    putStrLn $ "Parsed input: " ++ (Common.truncateString $ show parsedInput)

    -- Solve
    let answer1 = sum . map fst . filter snd . zip [1..]
                $ map (fromJust . (uncurry comparePacketLists)) parsedInput
    let dividers = [[PacketList [Packet 2]], [PacketList [Packet 6]]]
    let answer2 = product . map fst
                . filter ((`elem` dividers) . snd) . zip [1..]
                . sortBy (\a b -> case comparePacketLists a b of
                                    Just True -> LT
                                    Just False -> GT
                                    otherwise -> error "signals can't be equal")
                $ dividers ++ Common.flattenTuples2 parsedInput

    -- Print answers
    putStrLn $ "Part 1: " ++ show answer1
    putStrLn $ "Part 2: " ++ show answer2

data Packet = Packet Int
            | PacketList [Packet]
            deriving (Show,Eq)

comparePacketLists :: [Packet] -> [Packet] -> Maybe Bool
comparePacketLists [] [] = Nothing
comparePacketLists [] (_:_) = Just True
comparePacketLists (_:_) [] = Just False
comparePacketLists (a:as) (b:bs) =
    case (a,b) of
      (Packet a', Packet b') -> if a' == b' then comparePacketLists as bs else Just $ a' < b'
      (PacketList as', Packet b') -> case comparePacketLists as' [Packet b'] of
                                       Just result -> Just result
                                       Nothing -> comparePacketLists as bs
      (Packet a', PacketList bs') -> case comparePacketLists [Packet a'] bs' of
                                       Just result -> Just result
                                       Nothing -> comparePacketLists as bs
      (PacketList as', PacketList bs') -> case comparePacketLists as' bs' of
                                            Just result -> Just result
                                            Nothing -> comparePacketLists as bs

parse :: String -> [([Packet],[Packet])]
parse input = map toPair . splitOn [""] $ lines input
    where toPair [x,y] = (parseOne x, parseOne y)

parseOne :: String -> [Packet]
parseOne = go [] [] []
    where go _ ((PacketList signalBuffer):_) _ []            = signalBuffer
          go tmpBuffer signalBuffer _ ('[':xs)               = go (signalBuffer:tmpBuffer) [] [] xs
          go (t:mpBuffer) signalBuffer [] (']':xs)           = go mpBuffer (t ++ [PacketList signalBuffer]) [] xs
          go (t:mpBuffer) signalBuffer packetBuffer (']':xs) = go mpBuffer (t ++ [PacketList (signalBuffer ++ [Packet (read packetBuffer)])]) [] xs
          go tmpBuffer signalBuffer [] (',':xs)              = go tmpBuffer signalBuffer [] xs
          go tmpBuffer signalBuffer packetBuffer (',':xs)    = go tmpBuffer (signalBuffer ++ [Packet (read packetBuffer)]) [] xs
          go tmpBuffer signalBuffer packetBuffer (x:xs)      = go tmpBuffer signalBuffer (packetBuffer ++ [x]) xs
