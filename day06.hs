{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.List (findIndex, transpose)
import Data.Maybe (fromMaybe, catMaybes, mapMaybe)
import Debug.Trace (trace)
import Text.Read (readMaybe)

type Op = Int -> Int -> Int
type Chunk = (Op, [Int])
type Input = [[Char]]
type NumParser = Int -> [String] -> [Int]

chomper :: NumParser -> [Chunk] -> Input -> [Chunk]
chomper nParser chomped feed
    | opIndex == 0 = chomped
    | otherwise = chomper nParser ((op, nums) : chomped) $ map (drop (opIndex + 1)) feed
    where
        opIndex = fromMaybe 0 . findIndex (/= ' ') $ last feed
        op = (\case '+' -> (+); '*' -> (*)) $ last feed !! opIndex
        nums = nParser opIndex $ init feed

solve :: NumParser -> T.Text -> Int
solve nParser txt =
    sum . map (uncurry foldl1) . chomper nParser [] $ map reverse . lines . T.unpack $ txt

solvePart1 :: T.Text -> Int
solvePart1 = solve numParser
    where
        numParser idx = map (read . reverse . take (idx + 1))

solvePart2 :: T.Text -> Int
solvePart2 = solve numParser
    where
        numParser idx = mapMaybe readMaybe . transpose . map (take (idx + 1))

main :: IO ()
main = do
    args <- getArgs
    raw <- TIO.readFile $ head args

    putStrLn $ "Part 1: " <> show (solvePart1 raw) <> "\nPart 2: " <> show (solvePart2 raw)
