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
