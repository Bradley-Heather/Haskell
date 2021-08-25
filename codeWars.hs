module AlphabeticAnagrams where
import Data.List

lexiPos' :: String -> Int
lexiPos' xs = ((+1) . length) . fst $ break (== xs) $ sort . nub $ permutations xs

lexiPos []  = 0
lexiPos [x] = 1
lexiPos (x:xs) = length n + lexiPos n + lexiPos xs
    where n = filter (<x) xs
         
perm [] = 0
perm [n] = 1
perm (n:ns) = length (n:ns) * perm ns 


possPerm xs = product $ map perm $ group (sort xs)

perms xs = perm xs `div` possPerm xs