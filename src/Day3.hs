module Day3 where

import Data.List

santaHouses :: String -> Int
santaHouses = length . nub . parsePath
  where parsePath = scanr parseDirections (0, 0)

combinedHouses :: String -> Int
combinedHouses = length . nub . merge . foldedDirections
  where
    foldedDirections = foldl parse ([(0, 0)], [(0, 0)])
    merge (xs, ys) = xs ++ ys
    parse (xs, z:zs) dir = (y:z:zs, xs)
      where y = parseDirections dir z

parseDirections :: Char -> (Int, Int) -> (Int, Int)
parseDirections '>' (x, y) = (x+1, y)
parseDirections '<' (x, y) = (x-1, y)
parseDirections '^' (x, y) = (x, y+1)
parseDirections 'v' (x, y) = (x, y-1)

merge :: ([a], [a]) -> [a]
merge (xs, ys) = xs ++ ys

