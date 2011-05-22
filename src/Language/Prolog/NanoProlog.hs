{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Prolog.NanoProlog (
    Env
  , LowerCase
  , Result(..)
  , Rule((:<-:))
  , Term(..)
  , emptyEnv
  , subst
  , pFun
  , pList
  , pRule
  , pTerm
  , solve
  , startParse
  , unify
  ) where

import            Data.ListLike.Base (ListLike)
import            Data.List (intercalate)
import            Text.ParserCombinators.UU
import            Text.ParserCombinators.UU.BasicInstances
import            Text.ParserCombinators.UU.Utils


-- * Types
type UpperCase  = String
type LowerCase  = String

data Term  =  Var UpperCase
           |  Fun LowerCase [Term]
           deriving (Eq, Ord)

data Rule  =  Term :<-: [Term]
           deriving Eq

class Taggable a where
  tag :: Int -> a -> a

instance Taggable Term where
  tag n (Var  x)     = Var  (x ++ show n)
  tag n (Fun  x xs)  = Fun  x (tag n xs)

instance Taggable Rule where
  tag n (c :<-: cs) = tag n c :<-: tag n cs

instance Taggable a => Taggable [a] where
  tag n = map (tag n)

type Env = [(UpperCase, Term)]

emptyEnv :: Maybe Env
emptyEnv = Just []

-- * The Prolog machinery
data Result = None
            | Done Env 
            | ApplyRules [(Rule, Result)]

subst :: Env -> Term -> Term
subst env (Var x)     = maybe (Var x) (subst env) (lookup x env)
subst env (Fun x cs)  = Fun x (map (subst env) cs)

unify :: (Term, Term) -> Maybe Env -> Maybe Env
unify _       Nothing       = Nothing
unify (t, u)  env@(Just e)  = uni (subst e t) (subst e u)
  where  uni (Var x) y          = Just ((x, y): e)
         uni x       (Var y)    = Just ((y, x): e)
         uni (Fun x xs) (Fun y ys)
           | x == y && length xs == length ys  = foldr unify env (zip xs ys)
           | otherwise                         = Nothing

solve :: [Rule] -> Maybe Env -> Int -> [Term] -> Result
solve _      Nothing  _  _        =  None
solve _      (Just e)  _  []      =  Done e
solve rules  e  n  (t:ts)   
   =  ApplyRules [ (rule, solve rules (unify (t, c) e) (n+1) (cs ++ ts)) 
                 | rule@(c :<-: cs)   <- tag n rules 
                 ]

instance Show Term where
  show (Var  i)      = i
  show (Fun  i [] )  = i
  show (Fun  i ts )  = i ++ "(" ++ showCommas ts ++ ")"

instance Show Rule where
  show (t :<-: [] ) = show t ++ "."
  show (t :<-: ts ) = show t ++ ":-" ++ showCommas ts ++ "."

showCommas :: Show a => [a] -> String
showCommas l = intercalate ", " (map show l)

-- ** Parsing Rules and Terms
startParse :: (ListLike s b, Show b)  => P (Str b s LineColPos) a -> s
                                      -> (a, [Error LineColPos])
startParse p inp  =  parse ((,) <$> p <*> pEnd)
                  $  createStr (LineColPos 0 0 0) inp

pTerm, pVar, pFun :: Parser Term
pTerm  = pVar  <|>  pFun
pVar   = Var   <$>  lexeme (pList1 pUpper)
pFun   = Fun   <$>  pLowerCase <*> (pParens pTerms `opt` [])
       where pLowerCase :: Parser String
             pLowerCase = (:)  <$> pLower
                               <*> lexeme (pList (pLetter <|> pDigit))

pRule :: Parser Rule
pRule = (:<-:) <$> pFun <*> (pSymbol ":-" *> pTerms `opt` []) <* pDot

pTerms :: Parser [Term]
pTerms = pListSep pComma pTerm
