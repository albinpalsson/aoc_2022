module Main where

import AocShared (chunksOf, expect, ftrace, readLines)

main :: IO ()
main = do
  test <- readLines "test.txt"
  lines <- readLines "input.txt"
  expect (xregs ["noop", "addx 3", "addx -5"]) [1, 1, 1, 4, 4, -1]
  expect (part1 test) 13140
  expect (part1 lines) 13680 -- part1
  draw (litPixels test)
  draw (litPixels lines) -- part2

parse :: String -> [Int]
parse "noop" = [0]
parse line = [0, (read . last . words) line]

xregs :: [String] -> [Int]
xregs = (scanl (+) 1) . concat . (map parse)

part1 :: [String] -> Int
part1 lines = sum $ map sigStr [20, 60, 100, 140, 180, 220]
  where
    sigStr i = i * ((xregs lines) !! (i - 1))

litPixels :: [String] -> String
litPixels lines = map (uncurry hit) (zip sprites [0 ..])
  where
    hit sprite pixel = if (pixel `mod` 40) `elem` sprite then '@' else ' '
    sprites = map (\x -> [x - 1, x, x + 1]) (xregs lines)

draw :: String -> IO ()
draw = (mapM_ putStrLn) . (chunksOf 40)
