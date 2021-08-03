module Awesome.Numbers where

import Data.Char

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

isInteresting :: Int -> Answer
isInteresting a 
    | interestingNum a == True        = Yes
    | interestingNum (a + 1) == True  = Almost
    | interestingNum (a + 2) == True  = Almost
    | otherwise                      = No

interestingNum :: Int -> Bool
interestingNum x = x `rem` (10 ^ (length (show x) - 1)) == 0 || show x == reverse (show x) || isAscending x || isDescending x 
  
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