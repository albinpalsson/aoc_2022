module AocShared where

import Debug.Trace (trace)
import Lib ()
import qualified Data.Text as Text

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

type Grid a = [[a]]

data Coord = Coord {x :: Int, y :: Int} deriving (Show, Eq, Ord)

type Dir = (Coord -> Coord)

right, left, up, down :: Dir
right (Coord x y) = Coord (x + 1) y
left (Coord x y) = Coord (x - 1) y
up (Coord x y) = Coord x (y - 1)
down (Coord x y) = Coord x (y + 1)

gsize :: Grid a -> Coord
gsize g = Coord (length (g !! 0)) (length g)

at :: Grid a -> Coord -> a
at g c@(Coord x y) = (g !! y) !! x