module Types where

import qualified Control.Monad.Random as R (Rand, StdGen)

type Rand a = R.Rand R.StdGen a

-- Basic entities
type Chromosome = [Double]
type Population = [Chromosome]

type FitnessValue = Rational
type EvaluatedChromosome = (Chromosome, FitnessValue)
type EvaluatedPopulation = [EvaluatedChromosome]

type ChromosomePair = (Chromosome, Chromosome)
type MatedPopulation = [ChromosomePair]

type Solutions = [Population]

-- Operator functions
type InitializationFunction = Rand Population
type FitnessFunction = Chromosome -> FitnessValue
type EvaluationFunction = Population -> EvaluatedPopulation
type SelectionFunction = EvaluatedPopulation -> Rand Population
type CrossoverFunction = ChromosomePair -> Rand ChromosomePair
type MutationFunction = Chromosome -> Rand Chromosome

data OperatorSetup = OperatorSetup {
  -- |Provide initial population.
  initialization :: InitializationFunction,
  -- |Assign a fitness score to a chromosome.
  fitness :: FitnessFunction,
  -- |Evaluate current population's fitness scores.
  evaluation :: EvaluationFunction,
  -- |Randomly select new population, based on fitness scores.
  selection :: SelectionFunction,
  -- |Crossover two parents to produce two children.
  crossover :: CrossoverFunction,
  -- |Mutate a chromosome.
  mutation :: MutationFunction
}

data Configuration = Configuration {
  -- |Seed of the random generator
  seed :: Int,
  -- |Size of the population
  popSize :: Int,
  -- |Size of the feature vector of each chromosome
  chromSize :: Int,
  -- |Value range for each chromosome's feature
  range :: (Double, Double),
  -- |Maxinum number of epochs
  maxIterations :: Int
}
