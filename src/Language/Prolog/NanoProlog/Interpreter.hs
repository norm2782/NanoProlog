module Language.Prolog.NanoProlog.Interpreter where

import            Language.Prolog.NanoProlog.NanoProlog
import            Text.ParserCombinators.UU
import            System (getArgs)
import            System.IO

-- * Running the Interpreter
-- ** The main interpreter
-- | The `main` program prompt for a file with Prolog rules and call the main
-- interpreter loop
run :: IO ()
run =  do  args  <- getArgs
           fn    <- case args of
                      []     -> do  hSetBuffering stdin LineBuffering
                                    putStrLn "File with rules?"
                                    getLine
                      (x:_)  -> return x
           s     <- readFile fn
           let (rules, errors) = startParse (pList pRule) s
           if null errors  then  do  mapM_ print rules
                                     loop rules
                           else  do  putStrLn "No rules parsed"
                                     mapM_ print errors
                                     run

-- | `loop` ask for a goal, and enuartes all solutions found, each preceded by
-- a trace conatining the rules applied in a tree-like fashion
loop :: [Rule] -> IO ()
loop rules = do  putStrLn "goal? "
                 s <- getLine
                 unless (s == "quit") $
                   do  let (goal, errors) = startParse pFun s
                       if null errors
                         then  printSolutions (solve rules emptyEnv [("0",goal)])
                         else  do  putStrLn "Some goals were expected:"
                                   mapM_ print errors
                       loop rules

-- | `printSolutions` takes the result of a treewalk, which constructs
-- all the proofs, and pairs them with their final
-- substitutions. Alternative approaches in printing are to print the
-- raw proofs, i.e. without applying the final substitution (remove
-- the @subst env@ ). This nicely shows how the intermediate variables
-- come into life. By including the test on the length the facts
-- directly stemming from the data base are not printed. This makes
-- the proofs much shorter, but a bit less complete.
printSolutions ::  Result -> IO ()
printSolutions result = sequence_
  [  do  sequence_  [  putStrLn (prefix ++ " " ++ show (subst env pr))
                    |  (prefix, pr) <- reverse proof
                    ]
         putStr "substitution: "
         putStrLn (show env)
         void getLine
  |  (proof, env) <- enumerateDepthFirst [] result ]
