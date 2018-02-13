module Defaults where

import Types

defaultConfiguration :: Configuration
defaultConfiguration = Configuration {
  seed = 666,
  popSize = 10,
  chromSize = 10,
  range = (0, 10),
  maxIterations = 10
}
