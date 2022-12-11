module Main where

import AocShared (expect, ftrace, readLines)
import Data.List (foldl', foldl1', foldr, sort)

main :: IO ()
main = do
  print (turn (turn testinp 0) 1)
  print (playXRounds 20 (updateOp (`div` 3) testinp))
  expect (part1 testinp) 10605
  expect (part1 input) 117640
  expect (part2 (lcm' [23, 19, 13, 17]) testinp) 2713310158
  expect (part2 (lcm' [13, 11, 2, 5, 7, 3, 19, 17]) input) 30616425600

testinp =
  [ Monkey 0 [79, 98] (* 19) (divTest 23 2 3) 0,
    Monkey 1 [54, 65, 75, 74] (+ 6) (divTest 19 2 0) 0,
    Monkey 2 [79, 60, 97] (^ 2) (divTest 13 1 3) 0,
    Monkey 3 [74] (+ 3) (divTest 17 0 1) 0
  ]

input =
  [ Monkey 0 [63, 84, 80, 83, 84, 53, 88, 72] (* 11) (divTest 13 4 7) 0,
    Monkey 1 [67, 56, 92, 88, 84] (+ 4) (divTest 11 5 3) 0,
    Monkey 2 [52] (^ 2) (divTest 2 3 1) 0,
    Monkey 3 [59, 53, 60, 92, 69, 72] (+ 2) (divTest 5 5 6) 0,
    Monkey 4 [61, 52, 55, 61] (+ 3) (divTest 7 7 2) 0,
    Monkey 5 [79, 53] (+ 1) (divTest 3 0 6) 0,
    Monkey 6 [59, 86, 67, 95, 92, 77, 91] (+ 5) (divTest 19 4 0) 0,
    Monkey 7 [58, 83, 89] (* 19) (divTest 17 2 1) 0
  ]

divTest :: Integral a => a -> b -> b -> a -> (a, b)
divTest f alt1 alt2 i = if i `divisibleBy` f then (i, alt1) else (i, alt2)

data Monkey = Monkey
  { name :: !Int,
    items :: ![Int],
    operation :: !(Int -> Int),
    test :: !(Int -> (Int, Int)),
    activity :: !Int
  }

instance Show Monkey where
  show (Monkey n is _ _ ac) = "Monkey " ++ show n ++ " ac " ++ show ac ++ ": " ++ show is

divisibleBy :: Integral a => a -> a -> Bool
divisibleBy x y = x `mod` y == 0

turn :: [Monkey] -> Int -> [Monkey]
turn ms i = replace i (Monkey n [] op test (ac + length is)) (foldl' throw' ms (map (test . op) is))
  where
    Monkey n is op test ac = ms !! i
    throw' ms' (item, to) = throw item to ms'

throw :: Int -> Int -> [Monkey] -> [Monkey]
throw item to ms = replace to (Monkey n (is ++ [item]) op test ac) ms
  where
    Monkey n is op test ac = ms !! to

replace :: Int -> a -> [a] -> [a]
replace i m ms = take i ms ++ [m] ++ drop (i + 1) ms

playRound :: [Monkey] -> [Monkey]
playRound ms = foldl' turn ms [0 .. (length ms - 1)]

playXRounds :: Int -> [Monkey] -> [Monkey]
playXRounds x ms = iterate playRound ms !! x

updateOp :: (Int -> Int) -> [Monkey] -> [Monkey]
updateOp fun = map update
  where
    update (Monkey n is op test ac) = Monkey n is (fun . op) test ac

monkeyBusiness :: [Monkey] -> Int
monkeyBusiness = product . take 2 . reverse . sort . map (\(Monkey _ _ _ _ ac) -> ac)

part1 :: [Monkey] -> Int
part1 = monkeyBusiness . playXRounds 20 . updateOp (`div` 3)

lcm' :: [Int] -> Int
lcm' = foldl1' lcm

part2 :: Int -> [Monkey] -> Int
part2 lcmv = monkeyBusiness . playXRounds 10000 . updateOp (`mod` lcmv)
