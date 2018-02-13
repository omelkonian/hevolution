module Tests where

import Test.Hspec
import Test.QuickCheck

import Types
import Defaults (defaultConfiguration)
import Utilities (mate, unmate, update)
import Operators (defaultOperators)
import Runtime (randomRun)

tests :: IO ()
tests = hspec $ do
    -- Configuration
    let ?config = defaultConfiguration { popSize = 1, chromSize = 2 }
    let ?op = defaultOperators ?config

    let

    -- Utilities
    describe "mate" $
      it "should return list of pairs" $
        mate [[1.0], [2.0], [3.0], [4.0]] `shouldBe` [([1.0], [2.0]), ([3.0], [4.0])]
    describe "unmate" $
      it "should return the initial list" $
        unmate [([1.0], [2.0]), ([3.0], [4.0])] `shouldBe` [[1.0], [2.0], [3.0], [4.0]]
    describe "mate-unmate" $
      it "should cancel each other" $ property $
        \l -> unmate (mate l) == (l :: Population)
    describe "update" $
      it "should update element at given index" $
        update 1 6.66 [0.0, 1.0, 2.0] `shouldBe` [0.0, 6.66, 2.0]

    -- Operators
    describe "initialization" $
      it "should succeed in generating a random initial population" $
      randomRun (initialization ?op) `shouldBe` [[2.2149000296655066,4.301990652138232]]
    describe "fitness" $
      it "should properly calculate fitness of given chromosome" $ property $
        \c -> fitness ?op (c :: Chromosome) `shouldBe` 1.0
    describe "evaluation" $
      it "should properly evaluate given chromosome" $
        evaluation ?op [[0.0]] `shouldBe` [([0.0], 1.0)]
    describe "selection" $
      it "should select most fit chromosomes in a roulette-wheel manner" $
        randomRun (selection ?op $ zip [[0],[1],[2]] [1,1,1]) `shouldBe` [[2],[2],[0]]
    describe "crossover" $
      it "should properly crossover two given chromosomes" $
        randomRun (crossover ?op ([0, 1], [2, 3])) `shouldBe` ([0, 3], [2, 1])
    describe "mutation" $
      it "should properly mutate given chromosomes" $
        randomRun (mutation ?op [0, 1, 2, 3]) `shouldBe` [0,1.102749564257699,2,3]
