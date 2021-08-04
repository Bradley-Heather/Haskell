module Kata (balance) where

import Prelude hiding (Either(..))
-- import Preloaded(Comparison(..))
import Data.List

data Comparison = Left | Right | Balance deriving (Show, Eq, Enum, Bounded)

balance :: String -> String -> Comparison
balance left right 
    | weight left > weight right  = Left
    | weight left < weight right  = Right
    | otherwise                   = Balance 

weight :: String -> Int 
weight [] = 0 
weight n  = 3 * length (filter (== '?') n) + 2 * length (filter (== '!') n)

