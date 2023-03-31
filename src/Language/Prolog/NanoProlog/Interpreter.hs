module Language.Prolog.NanoProlog.Interpreter where

import            Language.Prolog.NanoProlog.NanoProlog
import            Language.Prolog.NanoProlog.Parser
import            Text.ParserCombinators.UU
import            System.Environment
import            System.IO
import            Data.List

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
-- a trace containing the rules applied in a tree-like fashion
loop :: [Rule] -> IO ()
loop rules = do
  putStrLn "goal? "
  s <- getLine
  unless (s == "quit") $
    do  let (goal, errors) = startParse pTerm s
        if null errors
          then  printSolutions (solve rules emptyEnv [("0",goal)])
          else  do  putStrLn "Some goals were expected:"
                    print  goal
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
printSolutions result =
  sequence_ (intersperse (putStr "next?" >> void getLine)
    [  do mapM_ (\(prefix, pr) ->  putStrLn (prefix ++ " " ++ show (subst env pr)))
                (reverse proof)
          putStr "substitution: "
          print env
    |  (proof, env) <- enumerateDepthFirst [] result ])
