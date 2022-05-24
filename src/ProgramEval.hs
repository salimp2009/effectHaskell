module ProgramEval where

numbersStartingAt :: Num t => t -> [t]
numbersStartingAt n =
            n: numbersStartingAt (n+1)