module Part1 where

import Text.Regex.Posix ((=~))

parseInputs :: String -> [[Int]]
parseInputs inputs =
  let r = lines inputs
      l = map (map read . words) r
   in l

-- let cmemory = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
part1Main :: IO ()
part1Main = do
  cmemory <- readFile "day03/input.txt"
  let matches = cmemory =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]]
  let val = sum [read x * read y | [_, x, y] <- matches]
  print val
