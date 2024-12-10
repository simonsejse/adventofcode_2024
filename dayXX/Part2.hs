module Part2 where

parseInputs :: String -> [[Int]]
parseInputs inputs =
  let r = lines inputs
      l = map (map read . words) r
   in l

part2Main :: IO ()
part2Main = do
  input <- readFile "dayXX/input.txt"
  print $ input
