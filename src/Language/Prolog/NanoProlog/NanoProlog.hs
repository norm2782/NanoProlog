module Language.Prolog.NanoProlog.NanoProlog (
     Env(..)
  ,  UpperCase
  ,  LowerCase
  ,  Tag
  ,  Result(..)
  ,  Rule((:<-:))
  ,  Subst(..)
  ,  Taggable(..)
  ,  Term(..)
  ,  Proofs
  ,  TaggedTerm
  ,  emptyEnv
  ,  enumerateDepthFirst
  ,  matches
  ,  solve
  ,  unify
  ) where

import            Data.List (intercalate)
import            Data.Map (Map)
import qualified  Data.Map as M

-- * Types
type UpperCase  = String
type LowerCase  = String
type Tag        = String

data Term  =  Var UpperCase
           |  Fun LowerCase [Term]
           deriving (Eq, Ord)

type TaggedTerm = (Tag, Term)

data Rule  =  Term :<-: [Term] deriving Eq

class Taggable a where
  tag :: Tag -> a -> a

instance Taggable Term where
  tag n (Var  x)     = Var  (x ++ n)
  tag n (Fun  x xs)  = Fun  x (tag n xs)

instance Taggable Rule where
  tag n (c :<-: cs) = tag n c :<-: tag n cs

instance Taggable a => Taggable [a] where
  tag n = map (tag n)

newtype Env = Env {fromEnv :: Map UpperCase Term}

emptyEnv :: Maybe Env
emptyEnv = Just (Env M.empty)

-- * The Prolog machinery
-- The result type contains a search tree, where the branches represent an application of a rule, and the 
-- leaves succesful results. Successes are  represented by their corresponding substitution.
-- A branch is represented by the tag used to loabel the rule that was applied, by the rule that was applied, 
-- and by the ``continution'' of the search.

data Result  =  Done Env
             |  ApplyRules [(Tag, Rule, Result)]

type Proofs = [(Tag, Rule)]

class Subst t where
  subst :: Env -> t -> t

instance Subst a => Subst [a] where
  subst e = map (subst e)

instance Subst Term where
  subst env (Var x)     = maybe (Var x) (subst env) (M.lookup x (fromEnv env))
  subst env (Fun x cs)  = Fun x (subst env cs)

instance Subst Rule where
  subst env (c :<-: cs) = subst env c :<-: subst env cs

matches :: (Term, Term) -> Maybe Env -> Maybe Env
matches _       Nothing        = Nothing
matches (t, u)  env@(Just e@(Env m))   = match(subst e t) u
  where  match  (Var x)     y  = Just . Env $ M.insert x y m
         match  (Fun x xs)  (Fun y ys)
           |  x == y && length xs == length ys = foldr matches env (zip xs ys)
         match  _           _  = Nothing

unify :: (Term, Term) -> Maybe Env -> Maybe Env
unify _       Nothing       = Nothing
unify (t, u)  env@(Just e@(Env m))  = uni (subst e t) (subst e u)
  where  uni  (Var x)  y        = Just  (Env (M.insert x  y  m))
         uni  x        (Var y)  = Just  (Env (M.insert y  x  m))
         uni  (Fun x xs) (Fun y ys)
           |  x == y && length xs == length ys  = foldr unify env (zip xs ys)
           |  otherwise                         = Nothing

solve :: [Rule] -> Maybe Env  -> [TaggedTerm] -> Result
solve _      Nothing   _            = ApplyRules []
solve _      (Just e)  []           = Done e
solve rules  e         ((tg,t):ts)  = ApplyRules
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
-- exercise to the  students
enumerateBreadthFirst :: Proofs -> Result -> [(Proofs, Env)]
-}

-- | `printEnv` prints a single solution, showing only the variables
-- that were introduced in the original goal
instance Show Env where
  show e@(Env env) = intercalate ", " . filter (not.null) . map showBdg $ M.assocs env
   where  showBdg (x, t)  | isGlobVar x  = x ++ " <- " ++ show(subst e t) 
                          | otherwise    = ""
          isGlobVar x = head x `elem` ['A'..'Z'] && last x `notElem` ['0'..'9']

instance Show Term where
  show  (Var  i)         = i
  show  (Fun  i []  )    = i
  show  (Fun "->"   [f@(Fun "->" _)  ,a]) = "(" ++ show f ++ ")" ++ " -> " ++ show a 
  show  (Fun "->"   [f               ,a]) =        show f ++        " -> " ++ show a 
  show  (Fun "cons" [h@(Fun "->"   _),t]) = "(" ++ show h ++ ")" ++ ":"    ++ show t 
  show  (Fun "cons" [h@(Fun "cons" _),t]) = "(" ++ show h ++ ")" ++ ":"    ++ show t 
  show  (Fun "cons" [h               ,t]) =        show h ++        ":"    ++ show t 
  show  (Fun "[]" [l])                    = "[" ++ show l ++ "]"
  show  (Fun  i ts  )                     = i ++ "(" ++ showCommas ts ++ ")"

instance Show Rule where
  show (t :<-: []  ) = show t ++ "."
  show (t :<-: ts  ) = show t ++ ":-" ++ showCommas ts ++ "."

showCommas :: Show a => [a] -> String
showCommas l = intercalate ", " (map show l)
