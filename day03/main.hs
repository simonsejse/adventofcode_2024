module Main where

import Part1
import Part2
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["part1"] -> part1Main
    ["p1"] -> part1Main
    ["part2"] -> part2Main
    ["p2"] -> part2Main
    _ -> putStrLn "Usage: cabal run day03 part1|part2"
