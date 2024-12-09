module Part2 where

import Data.Char (isDigit)

parseInputs :: String -> [[Int]]
parseInputs inputs =
  let r = lines inputs
      l = map (map read . words) r
   in l

data Token
  = TDon't
  | TDo
  | TMul Int Int
  deriving (Eq, Show)

lexer :: String -> [Token]
lexer [] = []
lexer input
  | take 7 input == "don't()" = TDon't : (lexer $ drop 7 input)
  | take 4 input == "do()" = TDo : (lexer $ drop 4 input)
  | take 4 input == "mul(" = (lexMul $ drop 4 input)
  | otherwise = (lexer $ drop 1 input) -- instead of tail input, maybe even drop 7+4?,

lexMul :: String -> [Token]
lexMul input =
  let (digit, remainder) = span isDigit input
      (comma, remainder') = span (== ',') remainder
      (digit', remainder'') = span isDigit remainder'
      (closeParen, remainder''') = span (== ')') remainder''
   in if null digit || null comma || null digit' || null closeParen
        then lexer remainder'''
        else TMul (read digit) (read digit') : lexer remainder'''

data Instruction = Do | Don't | Mul Int Int deriving (Eq, Show)

parser :: [Token] -> [Instruction]
parser [] = []
parser (TDon't : rest) = Don't : parser rest
parser (TDo : rest) = Do : parser rest
parser (TMul x y : rest) = Mul x y : parser rest

eval :: [Instruction] -> Int
eval instructions = eval' instructions True
  where
    eval' :: [Instruction] -> Bool -> Int
    eval' [] _ = 0
    eval' (Do : rest) _ = eval' rest True
    eval' (Don't : rest) _ = eval' rest False
    eval' (Mul x y : rest) True = x * y + eval' rest True
    eval' (Mul _ _ : rest) False = eval' rest False

part2Main :: IO ()
part2Main = do
  cmemory <- readFile "day03/input.txt"
  print $ eval $ parser $ lexer cmemory

-- let cmemory = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
