module Main where

import AocShared
import Data.Char
import Data.Maybe

main :: IO ()
main = do
  lines <- readLines "input.txt"
  print (parse example)
  expect (tallest (Coord 3 3) up (parse example)) 7
  expect (isVis (Coord 1 1) (parse example)) True
  expect (isVis (Coord 2 2) (parse example)) False
  expect (countVis (Coord 0 0) 0 (parse example)) 21
  expect (part1 lines) 1803
  expect (scenicScore (Coord 2 1) (parse example)) 4
  expect (scenicScore (Coord 2 3) (parse example)) 8
  expect (maxScenic (Coord 0 0) 0 (parse example)) 8
  expect (part2 lines) 268912

example = ["30373", "25512", "65332", "33549", "35390"]

type Forest = Grid Int

parse :: [[Char]] -> Forest
parse = map (map digitToInt)

outside :: Coord -> Forest -> Bool
outside (Coord x y) g = or [x < 0, x >= mx, y < 0, y >= my]
  where
    (Coord mx my) = gsize g

isVis :: Coord -> Forest -> Bool
isVis coord g = any visInDir [up, down, left, right]
  where
    visInDir dir = h > (tallest coord dir g)
    h = g `at` coord

tallest :: Coord -> Dir -> Forest -> Int
tallest pos@(Coord x y) dir g
  | next `outside` g = -1
  | otherwise = max (g `at` next) (tallest next dir g)
  where
    next = dir pos

countVis :: Coord -> Int -> Forest -> Int
countVis coord acc g
  | isJust next = countVis (fromJust next) acc' g
  | otherwise = acc'
  where
    next = iterNext coord g
    acc' = if isVis coord g then acc + 1 else acc

viewingDist :: Coord -> Int -> Dir -> Forest -> Int
viewingDist pos h dir g
  | next `outside` g = 0
  | (g `at` next) < h = (viewingDist next h dir g) + 1
  | otherwise = 1
  where
    next = dir pos

scenicScore :: Coord -> Forest -> Int
scenicScore coord g = product $ map vd [up, down, left, right]
  where
    vd dir = viewingDist coord h dir g
    h = g `at` coord

maxScenic :: Coord -> Int -> Forest -> Int
maxScenic c best g
  | isJust next = maxScenic (fromJust next) best' g
  | otherwise = best'
  where
    next = iterNext c g
    best' = max best (scenicScore c g)

iterNext :: Coord -> Forest -> Maybe Coord
iterNext c@(Coord x y) g
  | x < mx - 1 = Just (right c)
  | y < my - 1 = Just (Coord 0 (y + 1))
  | otherwise = Nothing
  where
    (Coord mx my) = gsize g

part1, part2 :: [[Char]] -> Int
part1 = (countVis (Coord 0 0) 0) . parse
part2 = (maxScenic (Coord 0 0) 0) . parse
