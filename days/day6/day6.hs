module Main where

import AocShared (expect)
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  contents <- readFile "input.txt"
  expect (findStart 0 4 "bvwbjplbgvbhsrlpgdmjqwftvncz") 5
  expect (findStart 0 4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") 11
  expect (findStart 0 4 contents) 1766 -- part 1
  expect (findStart 0 14 contents) 2383 -- part 2

findStart :: (Ord a) => Int -> Int -> [a] -> Int
findStart i size xxs@(x : xs)
  | length (Set.fromList (take size xxs)) == size = i + size
  | otherwise = findStart (i + 1) size xs
