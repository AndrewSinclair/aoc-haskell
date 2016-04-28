module Day3 where

import Data.List


santaHouses :: String -> Int
santaHouses directions = length $ nub $ scanr parseDirections (0, 0) directions

combinedHouses :: String -> Int
combinedHouses directions = length $ nub $ merge
      $ foldr parse ([(0, 0)], [(0, 0)]) directions
  where
    parse dir (xs, z:zs) = (y:z:zs, xs)
      where y = parseDirections dir z

parseDirections '>' (x, y) = (x+1, y)
parseDirections '<' (x, y) = (x-1, y)
parseDirections '^' (x, y) = (x, y+1)
parseDirections 'v' (x, y) = (x, y-1)


merge :: ([a], [a]) -> [a]
merge (xs, ys) = xs ++ ys
