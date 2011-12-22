{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Prolog.NanoProlog.Parser (
     pFun
  ,  pRule
  ,  pTerm
  ,  pCons
  ,  pTerms
  ,  startParse
  ) where

import            Data.ListLike.Base (ListLike)
import            Language.Prolog.NanoProlog.NanoProlog
import            Text.ParserCombinators.UU
import            Text.ParserCombinators.UU.BasicInstances
import            Text.ParserCombinators.UU.Utils

-- ** Parsing Rules and Terms
startParse :: (ListLike s b, Show b)  =>
              P (Str b s LineColPos) a -> s ->  (a, [Error LineColPos])
startParse p inp  =  parse ((,) <$> p <*> pEnd)
                  $  createStr (LineColPos 0 0 0) inp

pSepDot :: Parser String -> Parser [String]
pSepDot p = (:) <$> p <*> pList ((:) <$> pDot <*> p)

pTerm, pFactor, pCons, pVar, pFun :: Parser  Term
pTerm = pChainr ((\ f a -> Fun "->"   [f, a]) <$ pToken "->") pCons 
pCons = pChainr ((\ h t -> Fun "cons" [h, t]) <$ pToken ":" ) pFactor
pFactor  =     pVar
          <|>  pFun
          <|>  pParens pTerm

pVar   =      Var      <$>  lexeme ((++) <$> pList1 pUpper <*> (concat <$> pSepDot (pList1 pDigit) `opt` []))
pFun   =      Fun      <$>  pLowerCase <*> (pParens pTerms `opt` [])
         <|>  Fun "[]" <$>  pBrackets ((:[]) <$> pTerm)
  where  pLowerCase :: Parser String
         pLowerCase = lexeme ((:) <$> pLower <*> pList (pLetter <|> pDigit))

pRule :: Parser Rule
pRule = (:<-:) <$> pFun <*> (pSymbol ":-" *> pTerms `opt` []) <* pDot

pTerms :: Parser [Term]
pTerms = pList1Sep pComma pTerm

