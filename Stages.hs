module Stages where

import Control.Arrow ((>>>))

import Types as T
import Utilities (mate, unmate)

-- |A complete round of the GA.
generate :: (?op :: OperatorSetup) => Rand Population -> Rand Population
generate pop = do
  p <- pop
  sel <- selectionStage p
  cross <- crossoverStage sel
  mutationStage cross

-- |Select most fit solutions.
selectionStage :: (?op :: OperatorSetup) => Population -> Rand Population
selectionStage = evaluation ?op >>> selection ?op

-- |Crossover step.
crossoverStage :: (?op :: OperatorSetup) => Population -> Rand Population
crossoverStage = mate >>> sequence . fmap (crossover ?op) >>> fmap unmate

-- |Mutation step.
mutationStage :: (?op :: OperatorSetup) => Population -> Rand Population
mutationStage = sequence . fmap (mutation ?op)
