module Runtime where

import Control.Monad.Fix (fix)
import Control.Monad.Random (mkStdGen, evalRand)

import Stages (generate)
import Types

-- |Recursive generation of solutions.
feedback :: (?op :: OperatorSetup) => Rand Population -> Rand Solutions
feedback pop = sequence $ fix $ (pop :) . fmap generate

-- |Infinite list of solutions.
solutions :: (?op :: OperatorSetup) => Rand Solutions
solutions = feedback $ initialization ?op

-- |Iterate n times.
runN :: (?config :: Configuration, ?op :: OperatorSetup) => Rand Solutions
runN = take (maxIterations ?config) <$> solutions

-- |Iterate until a predicate holds for the population.
runWhile :: (?op :: OperatorSetup) => (Population -> Bool) -> Rand Solutions
runWhile p = takeWhile p <$> solutions

randomRun :: (?config :: Configuration, ?op :: OperatorSetup) => Rand a -> a
randomRun command = evalRand command $ mkStdGen (seed ?config)
