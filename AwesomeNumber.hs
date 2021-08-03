module Awesome.Numbers where

import Data.Char

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

isInteresting :: Integer -> [Integer] -> Answer
isInteresting a as
    | interestingNum a as                                    = Yes
    | interestingNum (a + 1) as || interestingNum (a + 2) as = Almost
    | otherwise                                              = No

interestingNum :: Integer -> [Integer] -> Bool
interestingNum x xs = x > 99 && (x `rem` (10 ^ (length (show x) - 1)) == 0 || show x == reverse (show x) || isAscending x || isDescending x || x `elem` xs)
  
count :: Integer -> [Integer]
count n = map toInteger $ map digitToInt $ show n 

correctCount :: [Integer] -> Bool
correctCount [x] = True
correctCount (x:n:ns) 
    | succ x == n = correctCount (n:ns)
    | otherwise = False

correctCount' :: [Integer] -> Bool
correctCount' [x] = True
correctCount' (x:n:ns) 
    | last (x:n:ns) == 0 && (x:n:ns) !! (length (x:n:ns) - 2) == 9 = correctCount' $ init (x:n:ns)
    | succ x == n                                                  = correctCount' (n:ns)
    | otherwise                                                    = False

isAscending :: Integer -> Bool
isAscending n = correctCount' $ count n

isDescending :: Integer -> Bool
isDescending n = correctCount $ reverse (count n)

-- alternate simplified solution

{-
{-# LANGUAGE ViewPatterns #-}
module Awesome.Numbers where
import Data.List

data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)

isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs | isMileage x                        = Yes
                   | isMileage (x+1) || isMileage (x+2) = Almost
                   | otherwise                          = No
  where isMileage (show -> y) = (length y >= 3) && any ($y) 
               [ all (== '0') . tail
               , all (== head y)
               , (`isInfixOf` "1234567890")
               , (`isInfixOf` "9876543210")
               , (== (reverse y))
               , (`elem` xs) . read
               ]

-}