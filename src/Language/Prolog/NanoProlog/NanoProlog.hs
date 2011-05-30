{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Prolog.NanoProlog.NanoProlog (
     Env
  ,  LowerCase
  ,  Result(..)
  ,  Rule((:<-:))
  ,  Subst(..)
  ,  Taggable(..)
  ,  Term(..)
  ,  emptyEnv
  ,  enumerateDepthFirst
  ,  pFun
  ,  pRule
  ,  pTerm
  ,  pTerms
  ,  show'
  ,  solve
  ,  startParse
  ,  unify
  ,  matches
  ) where

import            Data.ListLike.Base (ListLike)
import            Data.List (intercalate)
import            Data.Map (Map)
import qualified  Data.Map as M
import            Text.ParserCombinators.UU
import            Text.ParserCombinators.UU.BasicInstances
import            Text.ParserCombinators.UU.Utils

-- * Types
type UpperCase  = String
type LowerCase  = String
type Tag        = String

data Term  =  Var UpperCase
           |  Fun LowerCase [Term]
           deriving (Eq, Ord)

type TaggedTerm = (Tag, Term)

data Rule  =  Term :<-: [Term]
           deriving Eq

class Taggable a where
  tag :: Tag -> a -> a

instance Taggable Term where
  tag n (Var  x)     = Var  (x ++ n)
  tag n (Fun  x xs)  = Fun  x (tag n xs)

instance Taggable Rule where
  tag n (c :<-: cs) = tag n c :<-: tag n cs

instance Taggable a => Taggable [a] where
  tag n = map (tag n)

type Env = Map UpperCase Term

emptyEnv :: Maybe (Map UpperCase t)
emptyEnv = Just M.empty

-- * The Prolog machinery
data Result  =  Done Env
             |  ApplyRules [(Tag, Rule, Result)]

type Proofs = [(Tag, Rule)]

class Subst t where
  subst :: Env -> t -> t

instance Subst a => Subst [a] where
  subst e = map (subst e)

instance Subst Term where
  subst env (Var x)     = maybe (Var x) (subst env) (M.lookup x env)
  subst env (Fun x cs)  = Fun x (subst env cs)

instance Subst Rule where
  subst env (c :<-: cs) = subst env c :<-: subst env cs

matches :: (Term, Term) -> Maybe Env -> Maybe Env
matches _       Nothing       = Nothing
matches (t, u)  env@(Just m)  = match(subst m t) u
  where  match  (Var x)  y        = Just (M.insert x  y  m)
         match  (Fun x xs) (Fun y ys)
             |  x == y && length xs == length ys  = foldr matches env (zip xs ys)
         match _ _                = Nothing

unify :: (Term, Term) -> Maybe Env -> Maybe Env
unify _       Nothing       = Nothing
unify (t, u)  env@(Just m)  = uni (subst m t) (subst m u)
  where  uni  (Var x)  y        = Just (M.insert x  y  m)
         uni  x        (Var y)  = Just (M.insert y  x  m)
         uni  (Fun x xs) (Fun y ys)
           |  x == y && length xs == length ys  = foldr unify env (zip xs ys)
           |  otherwise                         = Nothing

solve :: [Rule] -> Maybe Env  -> [TaggedTerm] -> Result
solve _      Nothing   _        = ApplyRules []
solve _      (Just e)    []     = Done e
solve rules  e  ((tg,t):ts)  = ApplyRules
  [  let  cts = map ((tg ++) . ('.' :) . show) ([1..] :: [Int]) `zip` cs ++ ts
     in   (tg, rule, solve rules (unify (t, c) e) cts)
  |  rule@(c :<-: cs)  <- tag tg rules
  ]

-- ** Printing the solutions | `enumerateBreadthFirst` performs a
-- depth-first walk over the `Result` tree, while accumulating the
-- rules that were applied on the path which was traversed from the
-- root to the current node. At a successful leaf this contains the
-- full proof.
enumerateDepthFirst :: Proofs -> Result -> [(Proofs, Env)]
enumerateDepthFirst proofs (Done env)       = [(proofs, env)]
enumerateDepthFirst proofs (ApplyRules bs)  =
  [ s  |  (tag', rule, subTree) <- bs
       ,  s <- enumerateDepthFirst ((tag', rule):proofs) subTree
  ]

{-
-- | `enumerateBreadthFirst` is still undefined, and is left as an
-- exercise to the JCU students
enumerateBreadthFirst :: Proofs -> Result -> [(Proofs, Env)]
-}

-- | `printEnv` prints a single solution, showing only the variables
-- that were introduced in the original goal
show' :: Env -> String
show' env = intercalate ", " . filter (not.null) . map showBdg $ M.assocs env
  where  showBdg (x, t)  | isGlobVar x  = x ++ " <- " ++ showTerm t
                         | otherwise    = ""
         showTerm t@(Var _)   = showTerm (subst env t)
         showTerm (Fun f [])  = f
         showTerm (Fun f ts)  = f ++ "(" ++ intercalate ", " (map showTerm ts) ++ ")"
         isGlobVar x = head x `elem` ['A'..'Z'] && last x `notElem` ['0'..'9']

instance Show Term where
  show (Var  i)       = i
  show (Fun  i []  )  = i
  show (Fun  i ts  )  = i ++ "(" ++ showCommas ts ++ ")"

instance Show Rule where
  show (t :<-: []  ) = show t ++ "."
  show (t :<-: ts  ) = show t ++ ":-" ++ showCommas ts ++ "."

showCommas :: Show a => [a] -> String
showCommas l = intercalate ", " (map show l)

-- ** Parsing Rules and Terms
startParse :: (ListLike s b, Show b)  =>  P (Str b s LineColPos) a -> s
                                      ->  (a, [Error LineColPos])
startParse p inp  =  parse ((,) <$> p <*> pEnd)
                  $  createStr (LineColPos 0 0 0) inp

pSepDot :: Parser String -> Parser [String]
pSepDot p = (:) <$> p <*> pFoldr list_alg ((:) <$> pDot <*> p)

pTerm, pVar, pFun :: Parser Term
pTerm  = pVar  <|>  pFun
pVar   = Var   <$>  lexeme ((++) <$> pList1 pUpper <*> (concat <$> pSepDot (pList1 pDigit) <|> pure []))
pFun   = Fun   <$>  pLowerCase <*> (pParens pTerms `opt` [])
  where  pLowerCase :: Parser String
         pLowerCase = (:) <$> pLower <*> lexeme (pList (pLetter <|> pDigit))

pRule :: Parser Rule
pRule = (:<-:) <$> pFun <*> (pSymbol ":-" *> pTerms `opt` []) <* pDot

pTerms :: Parser [Term]
pTerms = pListSep pComma pTerm
