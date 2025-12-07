{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.List (findIndex)
import Data.Maybe (fromMaybe)

type Op = Int -> Int -> Int

charToOp :: Char -> (Int -> Int -> Int)
charToOp '+' = (+)
charToOp '*' = (*)

chomper :: [(Op, [Int])] -> [[Char]] -> [(Op, [Int])]
chomper chomped feed =
    let opIndex = fromMaybe 0 . findIndex (/= ' ') $ last feed
    in case opIndex of
        0 -> chomped
        _ ->
            chomper ((op, nums) : chomped) $ map (drop (opIndex + 1)) feed
            where
                op = charToOp $ last feed !! opIndex
                nums = map (read . reverse . take (opIndex + 1)) $ init feed

solvePart1 :: T.Text -> Int
solvePart1 txt =
    sum . map (uncurry foldl1) . chomper [] $ map reverse . lines . T.unpack $ txt

solvePart2 :: T.Text -> Int
solvePart2 _ = 0

printResults :: Int -> Int -> IO ()
printResults p1 p2 = do
  putStrLn ("Part 1: " <> show p1)
  putStrLn ("Part 2: " <> show p2)

readInput :: [String] -> IO T.Text
readInput [path] = TIO.readFile path

main :: IO ()
main = do
    args <- getArgs
    raw <- readInput args

    let p1 = solvePart1 raw
        p2 = solvePart2 raw

    printResults p1 p2
