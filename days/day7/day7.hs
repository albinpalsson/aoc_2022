module Main where

import AocShared (expect, ftrace, readLines)
import Data.List

data Item = File String Int | Dir String [Item] deriving (Show, Eq)

root = Dir "/" []

main :: IO ()
main = do
  test <- readLines "test.txt"
  print (fileSystem test)
  expect (itemSize (fileSystem test)) 48381165
  expect (part1 test) 95437
  expect (part2 test) 24933642
  lines <- readLines "input.txt"
  expect (part1 lines) 1206825
  expect (part2 lines) 9608311

parse :: [String] -> [String] -> Item -> Item
parse [] _ fs = fs
parse (l : lines) pwd fs
  | "$ cd /" == l = parse lines ["/"] fs
  | "$ cd .." == l = parse lines (init pwd) fs
  | "$ cd" `isPrefixOf` l = parse lines (pwd ++ [last (words l)]) fs
  | "$ ls" `isPrefixOf` l = parse (drop (length lsRes) lines) pwd updatedFs
  | otherwise = error ("cmd " ++ l)
  where
    updatedFs = fsUpdate pwd lsRes fs
    lsRes = lsParse lines []

lsParse :: [String] -> [Item] -> [Item]
lsParse [] items = items
lsParse (l : lines) items
  | "$" `isPrefixOf` l = items
  | "dir" `isPrefixOf` l = lsParse lines items ++ [dir]
  | otherwise = lsParse lines items ++ [file] -- file
  where
    file = File (split !! 1) (read (split !! 0))
    dir = Dir (split !! 1) []
    split = words l

fsUpdate :: [String] -> [Item] -> Item -> Item
fsUpdate [] items fs = fs
fsUpdate path items fs@(File _ _) = fs
fsUpdate path items fs@(Dir name ditems)
  | (head path) /= name = fs
  | length path == 1 = Dir name items
  | otherwise = Dir name (map (fsUpdate (tail path) items) ditems)

itemSize :: Item -> Int
itemSize (Dir n items) = sum $ map itemSize items
itemSize (File n size) = size

dirSizes :: Item -> [Int]
dirSizes (File _ _) = []
dirSizes d@(Dir n items) = [itemSize d] ++ concat (map dirSizes items)

fileSystem :: [String] -> Item
fileSystem lines = parse lines ["/"] root

part1, part2 :: [String] -> Int
part1 = sum . (filter (\x -> x <= 100000)) . dirSizes . fileSystem
part2 lines = head $ sort filtered
  where
    filtered = filter (\x -> x >= minReq) $ dirSizes fs
    minReq = (itemSize fs) - 40000000
    fs = fileSystem lines
