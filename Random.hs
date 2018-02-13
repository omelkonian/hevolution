module Random where

import Control.Monad (replicateM)
import qualified Control.Monad.Random as R (getRandomR, getRandomRs)

import Types

randIndex :: Chromosome -> Rand Int
randIndex c = R.getRandomR (0, length c - 1)

randPopulation :: (?config :: Configuration) => Rand Population
randPopulation = replicateM (popSize ?config) randChromosome

randChromosome :: (?config :: Configuration) => Rand Chromosome
randChromosome = take (chromSize ?config) <$> R.getRandomRs (range ?config)
