main = do  
        contents <- readFile "input.txt"
        print $ doPart1 contents
        print $ doPart2 contents

doPart1 = foldl (+) 0 . map readParens
doPart2 = length . takeWhile (>= 0) . scanl (+) 0 . map readParens

readParens :: Char -> Int
readParens paren = if paren == '(' then 1 else -1