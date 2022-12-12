module AocShared where

import Debug.Trace (trace)
import qualified Data.Text as Text
import Data.List (foldl')

expect :: (Eq a, Show a) => a -> a -> IO ()
expect a b
  | a == b = putStrLn "ok!"
  | otherwise = putStrLn $ "test failed, " ++ show a ++ " /= " ++ show b

ftrace :: c -> String -> c
ftrace = flip trace

readLines :: FilePath -> IO [String]
readLines fp = do
  contents <- readFile fp
  return $ lines contents

readLines' :: FilePath -> IO [Text.Text]
readLines' fp = do
  contents <- readFile fp
  return $ Text.lines $ Text.pack contents

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

splitOn :: String -> [Char] -> [String]
splitOn sep = foldl' gather [""]
  where
    gather prev c
      | c `elem` sep = prev ++ [""]
      | otherwise = init prev ++ [last prev ++ [c]]


----------- grid stuff... -----------

type Grid a = [[a]]

data Coord = Coord {x :: !Int, y :: !Int} deriving (Show, Eq, Ord)

type Dir = Coord -> Coord

right, left, up, down :: Dir
right (Coord x y) = Coord (x + 1) y
left (Coord x y) = Coord (x - 1) y
up (Coord x y) = Coord x (y - 1)
down (Coord x y) = Coord x (y + 1)

gsize :: Grid a -> Coord
gsize g = Coord (length (head g)) (length g)

at :: Grid a -> Coord -> a
at g (Coord x y) = g !! y !! x

inside :: Coord -> Grid a -> Bool
inside (Coord x y) g = and [x >= 0, y >= 0, x < mx, y < my]
  where
    Coord mx my = gsize g

gset :: Grid a -> Coord -> a -> Grid a
gset grid (Coord x y) v = take y grid ++ [updated] ++ drop (y + 1) grid
  where
    updated = take x row ++ [v] ++ drop (x + 1) row
    row = grid !! y

iterNext :: Coord -> Grid a -> Maybe Coord
iterNext c@(Coord x y) g
  | x < mx - 1 = Just (right c)
  | y < my - 1 = Just (Coord 0 (y + 1))
  | otherwise = Nothing
  where
    (Coord mx my) = gsize g
