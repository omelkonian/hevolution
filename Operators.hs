module Operators where

import Control.Monad (replicateM)
import Control.Monad.Random (getRandomR, fromList)

import Random
import Types
import Utilities

defaultOperators :: Configuration -> OperatorSetup
defaultOperators configuration = OperatorSetup {

  initialization = randPopulation,

  fitness = defaultFitness,

  evaluation = defaultEvaluation,

  selection = defaultSelection,

  crossover = defaultCrossover,

  mutation = defaultMutation

} where ?config = configuration

defaultFitness :: FitnessFunction
defaultFitness = const 1.0

defaultEvaluation :: EvaluationFunction
defaultEvaluation p = zip p $ map defaultFitness p

defaultSelection :: SelectionFunction
defaultSelection p = replicateM (length p) $ fromList p

defaultCrossover :: CrossoverFunction
defaultCrossover (c1, c2) = do
  index <- randIndex c1
  let (c11, c12) = splitAt index c1
  let (c21, c22) = splitAt index c2
  return (c11 ++ c22, c21 ++ c12)

defaultMutation :: (?config :: Configuration) => MutationFunction
defaultMutation c = do
  index <- randIndex c
  value <- getRandomR (range ?config)
  return $ update index value c
