module Part1 where

parseInputs :: String -> [[Int]]
parseInputs inputs =
  let r = lines inputs
      l = map (map read . words) r
   in l

part1Main :: IO ()
part1Main = do
  input <- readFile "dayXX/input.txt"
  print $ input
