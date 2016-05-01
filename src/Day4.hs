module Day4 where

import Data.Hash.MD5
import Data.List

mineAdvent input n = findHash
  where
    findHash = find  (hasPadding . md5s . doConcat) [1..]
    hasPadding str = n == (length $ takeWhile (== '0') str)
    doConcat i = Str (input ++ show i)

