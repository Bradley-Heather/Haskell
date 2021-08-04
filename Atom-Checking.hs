module MoleculeToAtoms where
import Text.Parsec
import Data.Char
import Data.Map (toList, unionsWith, fromList)

parseMolecule :: String -> Either String [(String, Int)]
parseMolecule formula = fst $ runParser expr formula

element = do
  a <- satisfy isUpper
  b <- many $ satisfy isLower
  return $ a : b

atoms = do
  e <- element
       <|> fail "Not a valid molecule"
  n <- parseDec <|> return 1
  return [(e, n)]

bracketed = do
  let p o c = bracket (isWord o) (commit $ isWord c) expr
              `adjustErr` (\_ -> "Mismatched parenthesis")
  g <- p "{" "}" <|>
       p "[" "]" <|>
       p "(" ")"
  i <- parseDec <|> return 1
  return [(x, n * i) | (x, n) <- g]

expr = do
  e <- some $ bracketed <|> atoms
  return $ toList . unionsWith (+) . map fromList $ e