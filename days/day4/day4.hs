module Main where

import AocShared (expect, ftrace, readLines, splitOn)
import Data.List

data Pair = Pair Int Int Int Int deriving (Show, Eq)

main :: IO ()
main = do
  lines <- readLines "input.txt"
  expect (parse "2-6,4-8") (Pair 2 6 4 8)
  expect (fullyCont (Pair 2 6 6 6)) True
  expect (part1 lines) 305 -- part 1
  expect (part2 lines) 811 -- part 2

parse :: String -> Pair
parse l = Pair (ints !! 0) (ints !! 1) (ints !! 2) (ints !! 3)
  where
    ints = map read (splitOn "-," l)

fullyCont :: Pair -> Bool
fullyCont (Pair lf ll rf rl) = or [null (l1 \\ l2), null (l2 \\ l1)]
  where
    l1 = [lf .. ll]
    l2 = [rf .. rl]

overlaps :: Pair -> Bool
overlaps (Pair lf ll rf rl) = not $ null (intersect [lf .. ll] [rf .. rl])

part1, part2 :: [String] -> Int
part1 = sum . map (fromEnum . fullyCont . parse)
part2 = sum . map (fromEnum . overlaps . parse)
