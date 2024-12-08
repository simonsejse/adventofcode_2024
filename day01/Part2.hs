module Part2 where

import Data.List
import Data.Maybe (fromMaybe)

parseInputs :: String -> ([Int], [Int])
parseInputs inputs =
  let rows = map (map read . words) (lines inputs)
      column1 = map head rows
      column2 = map (!! 1) rows
   in (column1, column2)

countOccurences :: [Int] -> [(Int, Int)]
countOccurences xs =
  let grouped = group (sort xs)
   in map (\x -> (head x, length x)) grouped

part2Main :: IO ()
part2Main = do
  contents <- readFile "day01/input.txt"
  let (xs_left, xs_right) = parseInputs contents
  let count_right = countOccurences xs_right
  print $ sum $ map (\x -> x * fromMaybe 0 (lookup x count_right)) xs_left
