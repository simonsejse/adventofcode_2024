module Part2 where

parseInputs :: String -> [[Int]]
parseInputs inputs =
  let r = lines inputs
      l = map (map read . words) r
   in l

isStrictlyIncreasing :: (Ord a, Num a) => [a] -> Bool
isStrictlyIncreasing [] = True
isStrictlyIncreasing [_] = True
isStrictlyIncreasing (x : y : xs)
  | x < y && isWithinRange y x = isStrictlyIncreasing (y : xs)
  | otherwise = False

isStrictlyDecreasing :: (Ord a, Num a) => [a] -> Bool
isStrictlyDecreasing [] = True
isStrictlyDecreasing [_] = True
isStrictlyDecreasing (x : y : xs)
  | x > y && isWithinRange x y = isStrictlyDecreasing (y : xs)
  | otherwise = False

isWithinRange :: (Ord a, Num a) => a -> a -> Bool
isWithinRange x y =
  let diff = x - y
   in 1 <= diff && diff <= 3

isStrictlyOrdered :: (Ord a, Num a) => [a] -> Bool
isStrictlyOrdered xs = isStrictlyIncreasing xs || isStrictlyDecreasing xs

leaveOneOut :: [a] -> [[a]]
leaveOneOut xs = [take i xs ++ drop (i + 1) xs | i <- [0 .. length xs - 1]]

part2Main :: IO ()
part2Main = do
  contents <- readFile "day02/input.txt"
  let reports = parseInputs contents
  let validReports = filter (\xs -> any isStrictlyOrdered (leaveOneOut xs)) reports
  print $ length validReports
