module Main where

import AocShared
import Data.Char (ord)
import Data.List (foldl', foldl1', intersect, sortBy)
import Data.Maybe (fromJust, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  testinp <- readLines "test.txt"
  lines <- readLines "input.txt"
  print (dijkstra testinp (parse testinp))
  expect (part1 testinp) 31
  expect (part1 lines) 472 -- part1
  expect (part2 testinp) 29
  expect (part2 lines) 465 -- part 2

data State = State
  { unvisited :: ![Coord],
    visited :: !(Set Coord),
    tentDist :: !(Grid Int)
  }
  deriving (Show)

parse :: Grid Char -> State
parse inp = State [start] Set.empty (gset initialTentDist start 0)
  where
    start = head (coordsOf 'E' inp)
    initialTentDist = map (map (\x -> maxBound :: Int)) inp

sortUnvisited :: [Coord] -> Grid Int -> [Coord]
sortUnvisited unv tdm = sortBy cmp unv
  where
    cmp a b = if (tdm `at` a) < (tdm `at` b) then LT else GT

dijkstra :: Grid Char -> State -> Grid Int
dijkstra _ (State [] _ td) = td
dijkstra g (State (v : unv) vis td) = dijkstra g (State unv' vis' td')
  where
    td' = foldl' updateNeigbour td unvisited
    unv' = sortUnvisited (unv ++ unvisited) td'
    updateNeigbour g c = gset g c ((td `at` v) + 1)
    unvisited = filter validate (map ($ v) [up, left, right, down])
    validate n = inside n g && valid (g `at` v) (g `at` n) && not (Set.member n vis)
    vis' = foldl' (flip Set.insert) vis unvisited

valid :: Char -> Char -> Bool
valid 'E' n = valid 'z' n
valid v 'S' = valid v 'a'
valid v n = ord n - ord v >= -1

coordsOfr :: Coord -> [Coord] -> Char -> Grid Char -> [Coord]
coordsOfr n xs c g
  | isNothing n' = xs
  | c == g `at` n = coordsOfr (fromJust n') (xs ++ [n]) c g
  | otherwise = coordsOfr (fromJust n') xs c g
  where
    n' = iterNext n g

coordsOf :: Char -> Grid Char -> [Coord]
coordsOf = coordsOfr (Coord 0 0) []

solve :: [String] -> Grid Int
solve lines = dijkstra lines (parse lines)

part1, part2 :: [String] -> Int
part1 lines = solve lines `at` head (coordsOf 'S' lines)
part2 lines = foldl1' min (map (solve lines `at`) (coordsOf 'a' lines))
