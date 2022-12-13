module Main where

import AocShared (chunksOf, expect, ftrace, readLines)
import Data.Char
import Data.List (findIndex, sort)
import Data.Maybe (fromJust)

main :: IO ()
main = do
  lines <- readLines "input.txt"
  testinp <- readLines "test.txt"

  print $ parseLine "[12,[1,2],3]"
  print $ parseLine "[10,[[1],22]]"
  expect (Value [Integer 3] < Integer 4) True
  expect (parseLine "[[1],[2,3,4]]" < parseLine "[[1],4]") True
  expect (parseLine "[7,7,7,7]" < parseLine "[7,7,7]") False

  expect (part1 testinp) 13
  expect (part1 lines) 5013

  expect (part2 testinp) 140
  expect (part2 lines) 25038

data Value = Integer !Int | Value ![Value] deriving (Show, Eq)

parseLine :: [Char] -> Value
parseLine line = v
  where
    (v, _) = parseValue line

parseValue :: [Char] -> (Value, Int)
parseValue [] = (Value [], 0)
parseValue pp@(p : packet)
  | p == '[' = (Value list, s + 1)
  | otherwise = (Integer (read digits :: Int), length digits)
  where
    (list, s) = parseList packet
    digits = getDigits pp

parseList :: [Char] -> ([Value], Int)
parseList [] = error "missing end of list"
parseList pp@(p : packet)
  | p == ']' = ([], 1)
  | p == ',' = ([], 1) `add` parseList packet
  | otherwise = ([v], s) `add` parseList (drop s pp)
  where
    (v, s) = parseValue pp

add :: ([Value], Int) -> ([Value], Int) -> ([Value], Int)
add (as, a) (bs, b) = (as ++ bs, a + b)

getDigits :: [Char] -> [Char]
getDigits [] = ""
getDigits (p : ps)
  | p == ',' || p == ']' = ""
  | otherwise = p : getDigits ps

instance Ord Value where
  compare (Value []) (Value []) = EQ
  compare (Value []) bs = LT
  compare as (Value []) = GT
  compare (Integer a) (Integer b) = compare a b
  compare a (Integer b) = compare a (Value [Integer b])
  compare (Integer a) b = compare (Value [Integer a]) b
  compare (Value (a : as)) (Value (b : bs)) = comphelp (compare a b) (compare (Value as) (Value bs))
    where
      comphelp LT f2 = LT
      comphelp EQ f2 = f2
      comphelp GT f2 = GT

parse :: [String] -> [(Value, Value)]
parse [] = []
parse lines = map (\ps -> (parseLine (head ps), parseLine (ps !! 1))) chunks
  where
    chunks = chunksOf 3 lines

part1 :: (Num a, Enum a) => [String] -> a
part1 lines = sum $ zipWith (\b i -> if b == LT then i else 0) compared [1 ..]
  where
    compared = map (uncurry compare) (parse lines)

div1, div2 :: Value
div1 = parseLine "[[2]]"
div2 = parseLine "[[6]]"

part2 :: [[Char]] -> Int
part2 lines = findV div1 * (findV div2 + 1)
  where
    sorted = (sort . map parseLine . filter (/= "")) lines
    findV v = fromJust (findIndex (v <) sorted) + 1
