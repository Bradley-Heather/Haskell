module Codewars.Kata.Permutations (permutations) where

import Data.List (delete, sort, group)

permutations :: String -> [String]
permutations xs = map head $ group . sort $ allPerms xs
   where
      allPerms [] = [[]] 
      allPerms xs = 
        do 
           x <- xs
           let n = delete x xs
           ns <- permutations n
           return (x : ns)
