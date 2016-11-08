-- A module that bundles all the offline learners, at the moment as an example contains
-- both classification and regression KNN.
module Learner(TrainingKnowledge, LearningParameters(..), LearnerParameters(..),
    train, classify, Learner.error) where

import Data.List
import Data.Ord
import Label
import Loss
import VectorUtils

-- LearningParameters data type represents the general parameters that each learner should supply,
-- such as the returned label type and the LearnerParameters (see the following comment) for the specfic learner.
data LearningParameters = LearningParameters LearnerParameters LabelType

-- LearnerParameters data type represents the specific parameters for each learner.
data LearnerParameters = KNN {
                k :: Int,
                norm :: Norm
               }

-- ExampleType data type represents the type of the examples that are given to the learner.
type ExampleType = [Double]

-- TrainingKnowledge data type represents the "knowledge" obtained by the training proccess
-- to be used when classifying a new example.
data TrainingKnowledge = KNNKnowledge [ExampleType] [Label] LearningParameters

-- A function that is used to achieve "knowledge" from given examples tagged by corresponding labels (ys)
-- to be used in the learning proccess
train :: LearningParameters -> [ExampleType] -> [Label] -> TrainingKnowledge
train classifier@(LearningParameters (KNN _  _)  _) xs ys = KNNKnowledge xs ys classifier

-- A function that is used to classify a new example using the "knowledge" obtained by the train proccess.
classify :: TrainingKnowledge -> ExampleType -> Label

-- classify for KNN regression
-- An average of the labels of the k closest neighbors among the training set
classify knowledge@(KNNKnowledge _ _ (LearningParameters knn DoubleType)) toClassify =
  LDouble (sum (map labelToDouble $ labelOfClosestNeighbors knowledge toClassify) / fromIntegral (k knn))

-- classify for KNN classification
-- Returns the label that appears the most among the k closest neighbors of the training set
classify knowledge@(KNNKnowledge _ _ (LearningParameters _ IntType)) toClassify = LInt result
  where
    result = snd $ maximumBy (comparing fst) $ zip (map length labelesGroupedByValue) labelsValues
    labelsValues = [head x | x <- labelesGroupedByValue]
    labelesGroupedByValue = group (map labelToInt $ labelOfClosestNeighbors knowledge toClassify)

-- A function that returns the labels of the k nearest neighbors
labelOfClosestNeighbors :: TrainingKnowledge -> ExampleType -> [Label]
labelOfClosestNeighbors (KNNKnowledge x y (LearningParameters knn _)) toClassify =
    snd $ unzip $ take (k knn) $ sort $ zip (distanceFromVector (norm knn) x toClassify) y

-- A function that is used to calculate the training error of the classifier
error :: TrainingKnowledge -> Loss -> [ExampleType] -> [Label] -> Double
error knowledge@(KNNKnowledge _ _ (LearningParameters _ _)) loss xs ys =
    sum [loss curY_hat curY | (curY_hat, curY) <- zip y_hat ys] / fromIntegral (length ys)
  where
    y_hat = map (classify knowledge) xs
