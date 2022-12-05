{-# LANGUAGE OverloadedStrings #-}

module Main where

import AocShared (expect, ftrace, readLines')
import Data.List (foldl', foldl1, splitAt)
import Data.Text (Text)
import qualified Data.Text as Text

data Instruction = Instruction Int Int Int deriving (Eq, Show)

main :: IO ()
main = do
  lines <- readLines' "input.txt"
  expect (parseIns "move 2 from 1 to 3") (Instruction 2 0 2)
  expect (updateStack 1 [1] [[1, 2], [2, 3], [3, 4]]) [[1, 2], [1], [3, 4]]
  expect (doMove [[1, 2], [2, 3], [3, 4]] (Instruction 1 1 0)) [[2, 1, 2], [3], [3, 4]]
  expect (doMove testInit (Instruction 1 1 0)) ["DNZ", "CM", "P"]
  expect (solve testInit testInp) "MCD"
  expect (solve initial lines) "VLCWHTDSZ"

parseIns :: Text -> Instruction
parseIns l = Instruction (nums !! 0) ((nums !! 1) - 1) ((nums !! 2) - 1)
  where
    nums = map (read . Text.unpack) [split !! 1, split !! 3, split !! 5]
    split = Text.splitOn " " l

doMove :: [[a]] -> Instruction -> [[a]]
doMove stacks (Instruction nr from to) = ((updateStack to nto) . (updateStack from nfrom)) stacks
  where
    nfrom = drop nr (stacks !! from)
    nto = (take nr (stacks !! from)) ++ (stacks !! to)

updateStack :: Int -> [a] -> [[a]] -> [[a]]
updateStack i n stacks = (b ++ [n]) ++ a
  where
    (b, _ : a) = splitAt i stacks

initial =
  [ ['W', 'B', 'G', 'Z', 'R', 'D', 'C', 'V'],
    ['V', 'T', 'S', 'B', 'C', 'F', 'W', 'G'],
    ['W', 'N', 'S', 'B', 'C'],
    ['P', 'C', 'V', 'J', 'N', 'M', 'G', 'Q'],
    ['B', 'H', 'D', 'F', 'L', 'S', 'T'],
    ['N', 'M', 'W', 'T', 'V', 'J'],
    ['G', 'T', 'S', 'C', 'L', 'F', 'P'],
    ['Z', 'D', 'B'],
    ['W', 'Z', 'N', 'M']
  ]

testInit = [['N', 'Z'], ['D', 'C', 'M'], ['P']]

testInp = ["move 1 from 2 to 1", "move 3 from 1 to 3", "move 2 from 2 to 1", "move 1 from 1 to 2"]

sumTop :: [[Char]] -> [Char]
sumTop stacks = foldl' sumTop [] stacks
  where
    sumTop b a = b ++ [(head a)]

solve :: [[Char]] -> [Text] -> String
solve init lines = sumTop (foldl' doMove init (map parseIns lines))
