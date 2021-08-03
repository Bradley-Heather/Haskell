module Awesome.Numbers where

import Data.Char

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)


isInteristing x =
    if isInteresting' x       == True  then  Yes
  --  if isInteresting' (x + 1) == True || isInteresting' (x + 2) == True then Almost
    else No

isInteresting' x = x `rem` (10 ^ (length (show x) - 1)) == 0 || show x == reverse (show x) || isAscending x || isDescending x 
  

count :: Int -> [Int]
count n = map digitToInt $ show n 

correctCount :: [Int] -> Bool
correctCount [x] = True
correctCount (x:n:ns) =
    if x + 1 == n then correctCount (n:ns)
    else False

isAscending :: Int -> Bool
isAscending n = correctCount $ count n

isDescending :: Int -> Bool
isDescending n = correctCount $ reverse (count n)