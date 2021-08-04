module Codewars.Kata.Hashtag where
import Data.Char


generateHashtag :: String -> Maybe String
generateHashtag n 
    | n /= [] && length n < 140 = Just ('#' : concatMap caps (words n))
    | otherwise                      = Nothing
        where caps (x:xs) = toUpper x : xs
    