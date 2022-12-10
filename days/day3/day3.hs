module Main where

import AocShared (chunksOf, expect, ftrace, readLines)
import Data.Char (isUpper, ord)
import Data.List (foldl', intersect)
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  lines <- readLines "input.txt"
  expect (split [1, 2, 3, 4]) ([1, 2], [3, 4])
  expect (common [1, 2, 4] [4, 1, 3]) [1, 4]
  expect (priority 'd') 4
  expect (priority 'P') 42
  expect (sumPrios ["ttgJtRGJQctTZtZT", "PmmdzqPrVvPwwTWBwg", "abcBAC"]) 62
  expect (sumPrios lines) 7903 -- part 1
  expect (rugSackGroupCommon ["wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"]) "Z"
  expect (sumThreeGroups lines) 2548 -- part 2

split :: [a] -> ([a], [a])
split xs = (take half xs, drop half xs)
  where
    half = (length xs) `div` 2

common :: (Eq a, Ord a) => [a] -> [a] -> [a]
common a b = Set.toList $ Set.fromList (intersect a b)

priority :: Char -> Int
priority c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1

linePrio :: String -> Int
linePrio l = sum $ map priority l

backpackPrio :: String -> Int
backpackPrio = linePrio . (uncurry common . split)

sumPrios :: [String] -> Int
sumPrios lines = sum $ map backpackPrio lines

rugSackGroupCommon :: [String] -> String
rugSackGroupCommon (x : xs) = foldl' common x xs

sumThreeGroups :: [String] -> Int
sumThreeGroups list = sum $ map linePrio threeGroups
  where
    threeGroups = map rugSackGroupCommon (chunksOf 3 list)
