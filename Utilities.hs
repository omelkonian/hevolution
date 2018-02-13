module Utilities where

import Data.List.Split (chunksOf)
import Data.Foldable (toList)
import qualified Data.Sequence as DS
import Control.Arrow

import Types

update :: Int -> Double -> Chromosome -> Chromosome
update index value =
  DS.fromList >>> DS.update index value >>> toList

mate :: Population -> MatedPopulation
mate = map (\l -> (l!!0, l!!1)) . chunksOf 2

unmate :: MatedPopulation -> Population
unmate = concatMap (\t -> [fst t, snd t])
