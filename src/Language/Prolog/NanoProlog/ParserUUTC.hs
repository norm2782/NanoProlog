module Language.Prolog.NanoProlog.ParserUUTC (
     pFun
  ,  pRule
  ,  pTerm
  ,  pCons
  ,  pTerms
  ,  startParse
  ) where

import            Control.Applicative ((<**>))
import            Language.Prolog.NanoProlog.NanoProlog
import            ParseLib.Abstract

spaces :: Parser Char String
spaces = many (choice [symbol ' ', symbol '\r', symbol '\n', symbol '\t'])

lexeme :: Parser Char a -> Parser Char a
lexeme p = p <* spaces

pDot :: Parser Char Char
pDot = symbol '.'

pSepDot :: Parser Char String -> Parser Char [String]
pSepDot p = (:) <$> p <*> many ((:) <$> pDot <*> p)

pChainr :: Parser s (a -> a -> a) -> Parser s a -> Parser s a
pChainr op x = r
  where r = x <??> (flip <$> op <*> r)

(<??>) :: Parser s b -> Parser s (b -> b) -> Parser s b
p <??> q = p <**> (q `opt` id)

pTerm, pFactor, pCons, pVar, pFun :: Parser Char Term
pTerm = pChainr ((\f a -> Fun "->"    [f, a]) <$ token "->") pCons
pCons = pChainr ((\h t -> Fun "cons"  [h, t]) <$ symbol ':') pFactor
pFactor =    pVar
        <|>  pFun
        <|>  parenthesised pTerm
pFun =    Fun       <$> pLowerCase <*> (parenthesised pTerms `opt` [])
     <|>  Fun "[]"  <$> bracketed ((:[]) <$> pTerm)
pVar = Var <$> lexeme ((++) <$> many1 pUpper <*> (concat <$> pSepDot (many1 pDigit) `opt` []))

pRange :: (Enum a, Eq a) => (a, a) -> Parser a a
pRange (b, e) = choice (map symbol [b..e])

pUpper, pLower, pLetter, pDigit :: Parser Char Char
pUpper = pRange ('A', 'Z')
pLower = pRange ('a', 'z')
pLetter = pUpper <|> pLower
pDigit = pRange ('0', '9')

pLowerCase :: Parser Char String
pLowerCase = lexeme ((:) <$> pLower <*> many (pLetter <|> pDigit))

pTerms :: Parser Char [Term]
pTerms = listOf pTerm (symbol ',')

pRule :: Parser Char Rule
pRule = (:<-:) <$> pFun <*> (token ":-" *> pTerms `opt` []) <* pDot

startParse :: Parser s a -> [s] -> [(a,[s])]
startParse p = parse (p <* eof)

opt :: Parser s a -> a -> Parser s a
opt p v = p <<|> pure v

