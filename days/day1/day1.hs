module Main where

import AocShared (expect, ftrace, readLines)
import Data.List (sort)
import Lib
import System.IO

main :: IO ()
main = do
  contents <- readLines "input.txt"
  expect (groupElves [] ["1000", "2000", "3000", "", "4000"]) [6000, 4000]
  expect (largest 1 [] [2, 1, 3]) [3]
  expect (solve 1 contents) 67633 -- part1
  expect (solve 3 ["1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000", "", "10000"]) 45000
  expect (solve 3 contents) 199628 -- part2

solve :: (Num a, Read a, Ord a) => Int -> [[Char]] -> a
solve topnr lines = sum $ largest topnr [] groups
  where
    groups = groupElves [] lines

groupElves :: (Num a, Read a) => [a] -> [[Char]] -> [a]
groupElves xs [] = xs
groupElves xs input = groupElves (xs ++ [total]) rest
  where
    total = sum (map read group)
    group = takeWhile (/= "") input
    rest = drop 1 (dropWhile (/= "") input)

largest :: Ord a => Int -> [a] -> [a] -> [a]
largest num set [] = set
largest num set (i : inp)
  | length set < num = largest num (sort (set ++ [i])) inp
  | i > head set = largest num (sort (tail set ++ [i])) inp
  | otherwise = largest num set inp
