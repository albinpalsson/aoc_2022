module Main where

import AocShared
import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  lines <- readLines "input.txt"
  test <- readLines "test.txt"
  print (follow (Coord 1 1) (Coord 1 2))
  print (follow (Coord 1 1) (Coord 1 3))
  print (((expand "R 4") !! 0) (Coord 0 0))
  print (update (State [(Coord 0 0), (Coord 0 0)] Set.empty) right)

  expect (part1 test) 13
  expect (part1 lines) 6209 -- part1
  print (follow' [Coord 1 1, Coord 0 0] (Coord 1 3))
  print (follow' [Coord 1 2, Coord 1 1] (Coord 1 4))

  expect (part2 test) 1
  expect (part2 lines) 2460 -- part2

data State = State {knots :: [Coord], visited :: Set Coord} deriving (Show)

touching :: Coord -> Coord -> Bool
touching (Coord x1 y1) (Coord x2 y2) = and [abs (x1 - x2) <= 1, abs (y1 - y2) <= 1]

follow :: Coord -> Coord -> Coord
follow t@(Coord tx ty) h@(Coord hx hy)
  | touching t h = t
  | xdiff == 0 = Coord tx (ty + norm ydiff)
  | ydiff == 0 = Coord (tx + norm xdiff) ty
  | otherwise = Coord (tx + norm xdiff) (ty + norm ydiff)
  where
    xdiff = hx - tx
    ydiff = hy - ty
    norm diff = diff `div` abs diff

follow' :: [Coord] -> Coord -> [Coord]
follow' [] _ = []
follow' (t : tail) h = [t'] ++ (follow' tail t')
  where
    t' = follow t h

update :: State -> Dir -> State
update s@(State (h : t) v) dir = State ([h'] ++ t') v'
  where
    h' = dir h
    t' = follow' t h'
    v' = Set.insert (last t') v

expand :: String -> [Dir]
expand l = replicate (read (w !! 1)) (parseDir (head w))
  where
    w = words l

parseDir :: String -> Dir
parseDir "U" = up
parseDir "D" = down
parseDir "L" = left
parseDir "R" = right
parseDir _ = error "unknown direction"

move :: State -> [String] -> Set Coord
move state@(State _ v) [] = v
move state (l : lines) = move (foldl' update state (expand l)) lines

part1, part2 :: [String] -> Int
part1 = length . move (State (replicate 2 (Coord 0 0)) Set.empty)
part2 = length . move (State (replicate 10 (Coord 0 0)) Set.empty)
