module Day5 where 

import Data.List

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = (f a) && (g a)

hasVowels :: String -> Bool
hasVowels = (>= 3) . length . (filter isVowel)
  where isVowel = (`elem` "aeiou")

hasDoubles :: String -> Bool
hasDoubles (x:y:xs) = x==y || (hasDoubles (y:xs))
hasDoubles (x:[]) = False
hasDoubles [] = False

containsForbidden :: String -> Bool
containsForbidden word = foldl (\acc digraph -> acc || digraph `isInfixOf` word) False forbiddenDigraphs
  where forbiddenDigraphs = ["ab", "cd", "pq", "xy"]

countNice :: [String] -> Int
countNice = length . filterConditions
  where filterConditions = filter (hasDoubles .&&. hasVowels .&&. (not . containsForbidden))


containsABA :: String -> Bool
containsABA [] = False
containsABA (x:[]) = False
containsABA (x:y:[]) = False
containsABA (x:y:z:xs) = x==z || (containsABA (y:z:xs))

containsTwoPair :: String -> Bool
containsTwoPair [] = False
containsTwoPair (x:[]) = False
containsTwoPair (x:y:xs) = (x:y:[]) `isInfixOf` xs || (containsTwoPair (y:xs))

countNiceBetter :: [String] -> Int
countNiceBetter = length . filterConditions
  where filterConditions = filter (containsABA .&&. containsTwoPair)

