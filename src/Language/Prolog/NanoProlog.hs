
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Prolog.NanoProlog (Term(..),Rule((:<-:)),LowerCase,unify,subst,startParse,pRule,pTerm) where

import            Data.ListLike.Base (ListLike)
import            Data.List (intercalate)
import            Text.ParserCombinators.UU
import            Text.ParserCombinators.UU.BasicInstances
import            Text.ParserCombinators.UU.Utils
import            System.IO


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

-- * Running the Interpreter
-- ** The main interpreter
-- | The `main` program prompt for a file with Prolog rules and call the main interpreter loop
main :: IO ()
main = do  hSetBuffering stdin LineBuffering
           putStr "File with rules? "
           fn  <- getLine
           s   <- readFile fn
           let (rules, errors) = startParse (pList pRule)  s
           if Prelude.null errors then  do  mapM_ print rules
                                            loop rules
                                  else  do  putStrLn "No rules parsed"
                                            mapM_ print errors
                                            main

-- | `loop` ask for a goal, and enuartes all solutions found, each preceded by a trace conatining the rules applied in a tree-like fashion
loop :: [Rule] -> IO ()
loop rules = do  putStr "goal? "
                 s <- getLine
                 unless (s == "quit") $
                   do  let (goal, errors) = startParse  pFun s
                       if null errors
                         then  printSolutions (print goal) ["0"] (solve rules  emptyEnv 0 [goal])
                         else  do  putStrLn "Some goals were expected:"
                                   mapM_ (putStrLn.show) errors
                       loop rules

-- ** Printing the solutions
-- | `printSolutions` performs a depth-first walk over the `Result` tree, while accumulating the rules that were applied on the path which was traversed from the root to the current node. At a successful leaf tis contains the full proof
printSolutions :: IO () -> [String] -> Result -> IO ()
printSolutions prProof _ (Done env)     = do prProof
                                             putStr "solution: "
                                             printEnv env
                                             getLine
                                             return ()
printSolutions _       _ None           = return ()
printSolutions prProof (pr:prefixes) (ApplyRules  bs) 
    = sequence_ [ printSolutions (prProof >> putStrLn  (pr ++ " " ++ show rule))  (extraPrefixes++prefixes) result 
                | (rule@(c :<-: cs), result) <-  bs
                , let extraPrefixes = take (length cs) (map (\i -> pr ++ "." ++ show i) [(1 :: Int) ..])
                ]

-- | `printEnv` prints a single solution, shwoing only the variables that were introduced in the original goal
printEnv :: Env -> IO ()
printEnv  bs =  putStr (intercalate ", " . filter (not.null) . map  showBdg $ bs)
             where  showBdg (    x,t)  | isGlobVar x =  x ++ " <- "++ showTerm t 
                                       | otherwise = ""   
                    showTerm t@(Var _)  = showTerm (subst bs t) 
                    showTerm (Fun f []) = f 
                    showTerm (Fun f ts) = f ++"("++ (intercalate ", " (map showTerm ts)) ++ ")"
                    isGlobVar x = head x `elem` ['A'..'Z'] && last x `notElem` ['0'..'9']   

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
