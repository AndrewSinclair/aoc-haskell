module Day4 where

import Data.Hash.MD5

mineAdvent input n = 
  case findHash of
   | Just val -> val
   | Nothing -> -1
  where
    findHash = find $ hasPadding . md5s . doConcat $ [1..]
    hasPadding = n == length $ takeWhile (== "0")
    doConcat i = input ++ show i

