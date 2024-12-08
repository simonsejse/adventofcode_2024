module Part1 where

import Data.List

parseInputs :: String -> ([Int], [Int])
parseInputs inputs =
  let rows = map (map read . words) (lines inputs)
      column1 = map head rows
      column2 = map (!! 1) rows
   in (column1, column2)

compareNumbers :: (Int, Int) -> Int
compareNumbers (elm1, elm2)
  | elm1 > elm2 = elm1 - elm2
  | otherwise = elm2 - elm1

part1Main :: IO ()
part1Main = do
  contents <- readFile "day01/input.txt"
  let (list1, list2) = parseInputs contents
  let column1 = sort list1
  let column2 = sort list2
  let zipped = zip column1 column2
  print $ sum (map compareNumbers zipped)
