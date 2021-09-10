{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module WordNumber where
import Data.List
import Data.Char
import Data.Text.Array (new)

data DividedResult = Result (Integer, Integer) | DividedByZero 
     deriving (Show, Eq)

divideBy :: Integer -> Integer -> DividedResult
divideBy 0 _ = DividedByZero
divideBy _ 0 = DividedByZero
divideBy num denom =  Result (go num denom 0)
   where go n d count 
          | n < d = (count, n)
          | otherwise =
                go (n - d) d (count + 1)

mc91 :: Integer -> Integer
mc91 n 
   | n > 100   = n - 10
   | otherwise = 91 

digitToWord :: Int -> String 
digitToWord n = ["Zero", "One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine"] !! n

digits :: Int -> [Int]
digits n = map digitToInt $ show n

wordNumber :: Int -> String
wordNumber n = intercalate "-" . map digitToWord $ digits n

safeTail :: [a] -> Maybe [a]
safeTail []    = Nothing 
safeTail [x]   = Nothing 
safeTail (_:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead []    =  Nothing 
safeHead (x:_) = Just x   

eftBool :: Bool -> Bool -> [Bool]
eftBool x y = enumFromTo False True

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = enumFromTo 

eftInt :: Int -> Int -> [Int]
eftInt n y = enumFromTo n y 

eftChar :: Char -> Char -> [Char]
eftChar = enumFromTo

td :: String -> [String]
td [] = []
td n  = if null dp then takeWhile (/=  ' ') n : td dp else takeWhile (/=  ' ') n : td (tail dp)
   where dp = dropWhile (/= ' ') n
      

firstSen = "Tiger Tiger, Burning Bright\n"
secondSen = "In the Forests of the Night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy Fearful symmetry"

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines [] = []
myLines n
   | null dp = takeWhile (/= '\n') n : myLines dp
   | otherwise = takeWhile (/= '\n') n : myLines (tail dp)
          where dp = dropWhile (/= '\n') n

words' _ [] = []
words' n xs 
   | null dp = takeWhile (/= n) xs : words' n dp
   | otherwise = takeWhile (/= n) xs : words' n (tail dp)
          where dp = dropWhile (/= n) xs

mySqr :: [Integer]
mySqr = [x^2 | x <- [1..5]]

myCube :: [Integer]
myCube = [y^3 | y <- [1..5]]

myTup ::  Int
myTup = length $ [(x, y) | x <- mySqr , y <- myCube, x < 50 , y < 50]

acro :: String -> String
acro xs = [x | x <- xs , x `elem` ['A'..'Z']] 

itIsAMystery :: [Char] -> [Bool]
itIsAMystery xs = map (\x -> x `elem` "aeiou") xs

myFilter ::  String -> [String]
myFilter = filter (\x -> x `notElem` ["The", "a", "an"]) . words

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (,) x y : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' n (x:xs) (y:ys) = n x y : zipWith' n xs ys 

caps :: String -> Char
caps = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = False 
myOr (x:xs) = x || myOr xs

myOr' :: [Bool] -> Bool
myOr' = foldr (\a b -> if a == True then True else b) False

myOr'' :: [Bool] -> Bool 
myOr'' = foldr (||) False 

myAny :: (a -> Bool) -> [a] -> Bool 
myAny _ [] = False 
myAny n (x:xs)
   | n x       = True 
   | otherwise = myAny n xs

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' n = foldr go False
    where go x y = n x || y

myElem :: Eq a => a -> [a] -> Bool 
myElem _ [] = False
myElem n (x:xs) 
    | x == n    = True 
    | otherwise = myElem n xs

myElem' :: Eq a => a -> [a] -> Bool 
myElem' n xs = any (== n) xs 

myElem'' :: Eq a => a -> [a] -> Bool
myElem'' n  = foldr go False 
     where go x y = n == x || y

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = undefined

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs 

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap n [] = []
squishMap n (x:xs) = n x ++ squishMap n xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

data Person = Person { name :: String 
                     , age :: Int 
                     }
                     deriving (Eq, Show)


type AuthorName = String 


data Author = Fiction AuthorName | NonFiction AuthorName 