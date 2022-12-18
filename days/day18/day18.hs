module Main where

import AocShared (expect, ftrace, readLines, splitOn)
import Data.List (foldl', foldl1')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  lines <- readLines "input.txt"
  test <- readLines "test.txt"
  expect (part1 test) 64
  expect (part1 lines) 3636
  expect (part2 test) 58
  expect (part2 lines) 2102

data Cube = Cube !Int !Int !Int deriving (Show, Eq, Ord)

data Box = Box
  { minX :: !Int,
    maxX :: !Int,
    minY :: !Int,
    maxY :: !Int,
    minZ :: !Int,
    maxZ :: !Int
  }
  deriving (Show)

parse :: [String] -> [Cube]
parse = map parseLine

parseLine :: String -> Cube
parseLine l = Cube (head split) (split !! 1) (split !! 2)
  where
    split = map read $ splitOn "," l

neighbours :: Cube -> [Cube]
neighbours (Cube x y z) =
  [ Cube (x + 1) y z,
    Cube (x - 1) y z,
    Cube x (y + 1) z,
    Cube x (y - 1) z,
    Cube x y (z + 1),
    Cube x y (z - 1)
  ]

neighbourCount :: Cube -> Set Cube -> Int
neighbourCount c all = length $ filter not (map find (neighbours c))
  where
    find c' = Set.member c' all

part1 :: [String] -> Int
part1 lines = foldl1' (+) (map (`neighbourCount` set) cubes)
  where
    set = Set.fromList cubes
    cubes = parse lines

makeBox :: [Cube] -> Box
makeBox cubes = Box minx maxx miny maxy minz maxz
  where
    Cube minx miny minz = foldl1' (cmpCubes min) cubes
    Cube maxx maxy maxz = foldl1' (cmpCubes max) cubes

cmpCubes :: (Int -> Int -> Int) -> Cube -> Cube -> Cube
cmpCubes cmp (Cube x y z) (Cube bx by bz) = Cube (cmp x bx) (cmp y by) (cmp z bz)

outside :: Cube -> Box -> Bool
outside (Cube x y z) (Box minx maxx miny maxy minz maxz) =
  or [x < minx, x > maxx, y < miny, y > maxy, z < minz, z > maxz]

dfs :: Cube -> Set Cube -> Map Cube Bool -> Box -> (Bool, Set Cube)
dfs c visited cache box
  | outside c box = (True, updatesVis)
  | Map.member c cache = (fromJust (Map.lookup c cache), visited)
  | null nonvisneigh = (False, updatesVis)
  | otherwise = foldl' comb (False, updatesVis) nonvisneigh
  where
    updatesVis = Set.insert c visited
    nonvisneigh = filter (not . (`Set.member` visited)) (neighbours c)
    combine (b1, s1) (b2, s2) = (b1 || b2, Set.union s1 s2)
    comb (b, vis') c' = combine (dfs c' vis' cache box) (b, vis')

add :: Ord k => a -> Set k -> Map k a
add v s = Map.fromList $ zip (Set.toList s) (map (const v) [0 ..])

updateMap :: Ord k => (a, Set k) -> Map k a -> Map k a
updateMap (b, s) m = Map.union m (add b s)

part2 :: [String] -> Int
part2 lines = length $ filter id $ map (fromJust . (`Map.lookup` searchAll)) all
  where
    cubes = parse lines
    box = makeBox cubes
    all = concatMap neighbours cubes
    init = add False (Set.fromList cubes)
    dfsfold m c = updateMap (dfs c Set.empty m box) m
    searchAll = foldl' dfsfold init all
