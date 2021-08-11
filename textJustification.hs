module TextAlignJustify where

import Data.List    (intercalate)
import Debug.Trace

justify :: String -> Int -> String
justify text width = let ws = getLines $ words text
                         l  = map printOneLine . init $ ws
                         r  = intercalate " "  . last $ ws
                     in intercalate "\n" $ l ++ [r]
  where
        getLines :: [String] ->[[String]]
        getLines [] = []
        getLines l  = let (line, rest) = getOneLine [] l in (line : getLines rest)

        getOneLine :: [String] -> [String] -> ([String], [String])
        getOneLine acc [] = (acc, [])
        getOneLine acc l@(x:xs)
          | length(intercalate " " $ acc ++ [x]) > width = (acc, l)
          | otherwise = getOneLine (acc ++ [x]) xs

        printOneLine [x] = x
        printOneLine current =
          let (space, rest) = divMod (width - length (concat current)) (length current - 1)
              (left, right) = splitAt (rest+1) current
              left' = intercalate (replicate (space+1) ' ') $ left
          in intercalate (replicate space ' ') $ left' : right