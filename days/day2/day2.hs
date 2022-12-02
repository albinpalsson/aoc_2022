{-# LANGUAGE OverloadedStrings #-}

module Main where

import AocShared (expect, ftrace, readLines')
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Lib
import System.IO

main :: IO ()
main = do
  lines <- readLines' "input.txt"
  expect (score1 "A Y") 8
  expect (score1 "B X") 1
  expect (score1 "C Z") 6
  expect (totalScore score1 lines) 13009
  expect (score2 "A Y") 4
  expect (score2 "B X") 1
  expect (score2 "C Z") 7
  expect (totalScore score2 lines) 10398

winScore :: Num a => Text.Text -> Text.Text -> a
winScore "A" "B" = 6
winScore "B" "C" = 6
winScore "C" "A" = 6
winScore a b
  | a == b = 3
  | otherwise = 0

normalize :: Text.Text -> Text.Text -> Text.Text
normalize _ "X" = "A"
normalize _ "Y" = "B"
normalize _ "Z" = "C"
normalize _ _ = "!"

choiceScore :: (Num a) => Text.Text -> a
choiceScore "A" = 1
choiceScore "B" = 2
choiceScore _ = 3

normalize2 :: Text.Text -> Text.Text -> Text.Text
normalize2 x "X" = Map.findWithDefault "!" x (Map.fromList [("A", "C"), ("B", "A"), ("C", "B")])
normalize2 x "Z" = Map.findWithDefault "!" x (Map.fromList [("A", "B"), ("B", "C"), ("C", "A")])
normalize2 x "Y" = x
normalize2 _ _ = "!"

totalScore :: Num a1 => (a2 -> a1) -> [a2] -> a1
totalScore scoreFun xs = sum $ map scoreFun xs

score1 :: Text.Text -> Integer
score1 = score normalize

score2 :: Text.Text -> Integer
score2 = score normalize2

score :: Num a => (Text.Text -> Text.Text -> Text.Text) -> Text.Text -> a
score normFun s = choice + result
  where
    choice = choiceScore playerChoice
    result = winScore (head game) playerChoice
    playerChoice = normFun (head game) (last game)
    game = Text.splitOn " " s
