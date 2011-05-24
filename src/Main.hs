module Main where

import            Language.Prolog.NanoProlog.Interpreter (run)

-- * Running the Interpreter
-- ** The main interpreter
-- | The `main` program prompt for a file with Prolog rules and call the main
-- interpreter loop
main :: IO ()
main = run
