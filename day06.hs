{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Data.List (findIndex, transpose, unfoldr)
import Data.Maybe (fromMaybe, mapMaybe)
import Text.Read (readMaybe)

type Operator = Int -> Int -> Int
type Chunk = (Operator, [Int])
type NumParser = Int -> [String] -> [Int]

chunker :: NumParser -> [String] -> Maybe (Chunk, [String])
chunker nParser feed
    | opIndex == 0 = Nothing
    | otherwise = Just ((op, nums), rest)
    where
        opIndex = fromMaybe 0 . findIndex (/= ' ') $ last feed
        op = (\case '+' -> (+); '*' -> (*)) $ last feed !! opIndex
        nums = nParser opIndex $ init feed
        rest = map (drop (opIndex + 1)) feed

solve :: NumParser -> T.Text -> Int
solve nParser txt =
    sum . map (uncurry foldl1) . unfoldr (chunker nParser) $ map reverse . lines . T.unpack $ txt

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
