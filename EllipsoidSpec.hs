module EllipsoidSpec where
-- This module contains tests for the ellipsoid learner

import Label
import Writer
import Loss
import Test.Hspec
import OnlineLearner
import Data.Matrix

-- The Ellipsoid knowledge contains fractions, like eta, a and w. This can cause
-- a problem when comparing two ellipsoids which should contain the same values,
-- but because of a different rounding appear to be different.
-- This function rounds all the fractions in the knowledge to N digits to enable comparison.
roundKnowledgeToNDigits :: Int -> TrainingKnowledge -> TrainingKnowledge
roundKnowledgeToNDigits digits (EllipsoidKnowledge d eta' a' w') = EllipsoidKnowledge d roundedEta roundedA roundedW
  where
    roundToNDigits n num =  fromInteger (round $ num * (10 ^ n)) / (10.0 ^^ n)
    roundMapToNDigits n = fmap (roundToNDigits n)
    roundedEta = roundToNDigits digits eta'
    roundedA = roundMapToNDigits digits a'
    roundedW = roundMapToNDigits digits w'

-- This function rounds two knowledges and compares them using `shouldBe`
roundAndCompare :: TrainingKnowledge -> TrainingKnowledge -> Expectation
roundAndCompare k1 k2 = roundKnowledgeToNDigits precision k1 `shouldBe` roundKnowledgeToNDigits precision k2
  where precision = 7

-- The main runs all the tests
main :: IO ()
main = do
    let eta' = 4 / 3

    let x1 = fromList 2 1 [1, 0]
    let y1 = LInt 1

    let x2 = fromList 2 1 [3, 3]
    let y2 = LInt (-1)

    let x3 = fromList 2 1 [0.3, 0.5]
    let y3 = LInt 1

    let x4 = fromList 2 1 [1, 1]
    let y4 = LInt 1

    let x5 = fromList 2 1 [9, 100]
    let y5 = LInt (-1)

    let x6 = fromList 2 1 [-10, -5]
    let y6 = LInt (-1)

    let x7 = fromList 2 1 [-0.01, -0.05]
    let y7 = LInt 1

    let xs = [x1, x2, x3, x4]
    let ys = [y1, y2, y3, y4]

    let Writer (initialKnowledge, initLog) = initKnowledge (EllipsoidParameters 2)
    let Writer (trainKnowledge, trainLog) = train initialKnowledge x1 y1
    let Writer (batchTrainKnowledge, batchTrainLog) = batch initialKnowledge [x1] [y1]
    let Writer (classifyKnowledge, classifyLog) = batch initialKnowledge xs ys

    let t1KnowA = fromList 2 2 [4 / 9, 0, 0, 4 / 3]
    let t1KnowW = fromList 1 2 [1 / 3, 0]
    let train1Knowledge = EllipsoidKnowledge 2 eta' t1KnowA t1KnowW

    hspec $ do
      describe "Init tests" $
        it "Initialization test" $
          roundAndCompare initialKnowledge (EllipsoidKnowledge 2 eta' (identity 2) (zero 1 2))

      describe "Train tests" $
        it "Train on one example" $
          roundAndCompare trainKnowledge train1Knowledge

      describe "Batch tests" $ do
        it "Train on one example" $
          roundAndCompare batchTrainKnowledge train1Knowledge

        it "Train on a lot examples" $ do
          let a' = fromList 2 2 [0.65843621, -0.39506173,
                                -0.39506173, 0.55308642]
          let w' = fromList 1 2 [0.22222222, -0.05224199]
          let classifyKnowledge_hat = EllipsoidKnowledge 2 eta' a' w'
          roundAndCompare classifyKnowledge classifyKnowledge_hat

      describe "Classify tests" $
        it "Classify examples" $ do
          getValFromWriter (classify classifyKnowledge x5) `shouldBe` y5
          getValFromWriter (classify classifyKnowledge x6) `shouldBe` y6
          getValFromWriter (classify classifyKnowledge x7) `shouldBe` y7

      describe "Errors tests" $
        it "Train on one example" $ do
          let xsToTest = [x5, x6, x7]
          let ysToTest = [y5, y6, y7]
          let errorRate = OnlineLearner.error classifyKnowledge binaryLoss xsToTest ysToTest
          getValFromWriter errorRate `shouldBe` 0

    let logs = initLog `mappend` trainLog `mappend` batchTrainLog `mappend` classifyLog
    putStrLn "------Done Testing------\nCallStack log:"
    putStrLn $ fromDiffList logs
