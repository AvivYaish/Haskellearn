module KNNSpec where
-- This module contains tests for the KNN learner

import Test.Hspec
import Learner
import Label
import Loss
import VectorUtils

-- Run all the tests
main :: IO ()
main = hspec $ do
  let norm2 = lNorm 2

  let x1 = [1, 1]
  let x2 = [1, 2]
  let x3 = [1, 3]
  let x4 = [1, 4]
  let x5 = [1, 5]
  let x6 = [1, 0]

  let xs = [x1, x2, x3, x4, x5]

  describe "Regression" $ do
    it "Regression test" $ do
      let classifier = LearningParameters KNN {k = 2, norm = norm2} DoubleType
      let ys = [LDouble 1, LDouble 0]
      classify (train classifier [x1, x2] ys) x3 `shouldBe` LDouble 0.5

    let ys = [LDouble 1, LDouble 1, LDouble 1, LDouble 0, LDouble 1]
    let params = LearningParameters KNN {k = 4, norm = norm2} DoubleType
    let knowledge = train params xs ys
    it "Regression complicated test" $
      classify knowledge x3 `shouldBe` LDouble (3 / 4)

    it "Error test with quadriatic loss" $
      Learner.error knowledge quadriaticLoss xs ys `shouldBe` (13 / 80)

  describe "Classification" $ do
    let ys = [LInt 1, LInt 0, LInt 1, LInt 0, LInt 1]

    it "Classification test" $ do
        let params = LearningParameters KNN {k = 1, norm = norm2} IntType
        let knowledge = train params [x1, x2] [LInt 1, LInt 0]
        classify knowledge x3 `shouldBe` LInt 0

    it "Classification complicated test" $ do
        let params = LearningParameters KNN {k = 4, norm = norm2} IntType
        let knowledge = train params xs ys
        classify knowledge x6 `shouldBe` LInt 0

    let complicatedParams = LearningParameters KNN {k = 5, norm = norm2} IntType
    let complicatedKnowledge = train complicatedParams xs ys
    it "Classification complicated test 2" $
      classify complicatedKnowledge x6 `shouldBe` LInt 1

    it "Error test with binary loss" $ do
      Learner.error complicatedKnowledge binaryLoss [x6] [LInt 1] `shouldBe` 0
      Learner.error complicatedKnowledge binaryLoss [x6] [LInt 0] `shouldBe` 1
      Learner.error complicatedKnowledge binaryLoss [x5, x6] [LInt 1, LInt 1] `shouldBe` 0

    it "Error test with binary loss and k=1" $ do
      let params = LearningParameters KNN {k = 1, norm = norm2} IntType
      let knowledge = train params xs ys
      Learner.error knowledge binaryLoss xs ys `shouldBe` 0
