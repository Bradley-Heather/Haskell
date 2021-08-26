stops :: [Char]
stops  = "pbtdkg"

vowels :: [Char]
vowels = "aeiou"

funWrds :: [(Char, Char, Char)]
funWrds = [(s1 , v , s2) | s1 <- stops, v <- vowels, s2 <- stops]

funwrds' :: [(Char, Char, Char)]
funwrds' = [('p', 'a', s2) | s2 <- stops ]

nouns = ["cat", "dog", "ball", "flamingo", "hat"]
verbs = ["has", "on", "under", "eats"]

funWrds'' :: [String]
funWrds'' = [ n1 ++ " " ++ v ++ " " ++ n2 | n1 <- nouns, v <- verbs, n2 <- nouns]

seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))