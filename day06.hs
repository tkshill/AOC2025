{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)

type Op = Int -> Int -> Int

charToOp :: Char -> (Int -> Int -> Int)
charToOp = \case '+' -> (+); '*' -> (*)

chomper :: [(Op, [Int])] -> [[Char]] -> [(Op, [Int])]
chomper chomped feed
    | opIndex == 0 = chomped
    | otherwise = chomper ((op, nums) : chomped) $ map (drop (opIndex + 1)) feed
    where
        opIndex = fromMaybe 0 . findIndex (/= ' ') $ last feed
        op = charToOp $ last feed !! opIndex
        nums = map (read . reverse . take (opIndex + 1)) $ init feed

solvePart1 :: T.Text -> Int
solvePart1 txt =
    sum . map (uncurry foldl1) . chomper [] $ map reverse . lines . T.unpack $ txt

solvePart2 :: T.Text -> Int
solvePart2 _ = 0

main :: IO ()
main = do
    args <- getArgs
    raw <- TIO.readFile $ head args

    let printResults p1 p2 = do
          putStrLn $ "Part 1: " <> show p1 <> "\nPart 2: " <> show p2

    printResults (solvePart1 raw) (solvePart2 raw)
