module Main where

import            Language.Prolog.NanoProlog
import            Data.List (intercalate)
import            Text.ParserCombinators.UU
import            System.IO

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
