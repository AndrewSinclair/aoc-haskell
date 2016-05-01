import Data.List
import Data.List.Split

main = do  
        content <- readFile "input.txt"
        print $ calcWrappingTotal $ map parsePresent $ lines content
        print $ calcRibbonTotal   $ map parsePresent $ lines content
        where
          parsePresent = map (read :: String -> Int) . splitOn "x"

calcWrappingTotal :: [[Int]] -> Int
calcWrappingTotal presents = sum $ map wrappingAmount presents
                    where 
                      wrappingAmount present =  area present + slack present
                      area (l:w:h:[]) = 2 * (l * h  + h * w + l * w)
                      slack = foldr1 (*) . take 2 . sort

calcRibbonTotal :: [[Int]] -> Int
calcRibbonTotal presents = sum $ map ribbonAmount presents
                  where
                    ribbonAmount present = perimeter present + volume present
                    perimeter = (* 2) . sum . take 2 .sort
                    volume = foldr1 (*) 
