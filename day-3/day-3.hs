import Data.List

main = do
        directions <- readFile "input2.txt"
        print $ santaHouses directions
        print $ combinedHouses directions

santaHouses :: String -> Int
santaHouses directions = length $ nub $ scanr parseDirections (0, 0) directions
  where
    parseDirections dir (x,y) = case dir of
                                  '>' -> (x+1, y)
                                  '<' -> (x-1, y)
                                  'v' -> (x, y+1)
                                  '^' -> (x, y-1)

combinedHouses :: String -> Int
combinedHouses directions = length $ nub $ merge $ foldr parseCombinedDirections ([(0, 0)], [(0, 0)]) directions
  where
    parseCombinedDirections dir (xs, z:zs) = ((updateDir dir z):z:zs, xs)
    updateDir dir (x, y) = case dir of
                            '>' -> (x+1, y)
                            '<' -> (x-1, y)
                            'v' -> (x, y+1)
                            '^' -> (x, y-1)

merge :: ([a], [a]) -> [a]
merge ([], ys) = ys
merge (x:xs, ys) = x:(merge (ys, xs))
