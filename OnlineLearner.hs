module OnlineLearner(TrainingKnowledge(..), LearnerParameters(..),
    initKnowledge, train, classify, batch, OnlineLearner.error) where

import Label
import Writer
import Loss
import Data.Matrix

-- Example represents an example that online learners can receive
type Example = Matrix Double

-- LearnerParameters data type represents the specific parameters for each learner.
data LearnerParameters = EllipsoidParameters Int deriving Show

-- TrainingKnowledge data type represents the "knowledge" obtained by the
-- training proccess to be used when classifying a new example.
data TrainingKnowledge = EllipsoidKnowledge {
                            dimension :: Int,
                            eta :: Double,
                            a :: Matrix Double,
                            w:: Matrix Double
                          } deriving Eq

instance Show TrainingKnowledge where
  show (EllipsoidKnowledge d eta' a' w') = "EllipsoidKnowledge with parameters: " ++
    "\n\tdimension: " ++ show d ++ "\n\teta: " ++ show eta' ++ "\n\tw:\n\t"  ++
    show w' ++ "\tA:\n" ++ showIndent a'

-- Initializes a "knowledge" to the default values for the specific learner.
initKnowledge :: LearnerParameters -> Writer (DiffList Char) TrainingKnowledge
initKnowledge (EllipsoidParameters d) = do
    let dDouble = fromInteger $ toInteger d
    let eta' =  dDouble * dDouble / (dDouble * dDouble - 1) :: Double
    let res = EllipsoidKnowledge d eta' (identity d) (zero 1 d)
    tell $ toDiffList $ "Called initKnoweledge with dimension: " ++ show d ++
      " and the result was:\n" ++ show res  ++ "\n\n"
    return res

-- Given a knowledge and a labeled example, trains the learner on it
train :: TrainingKnowledge -> Example -> Label -> Writer (DiffList Char) TrainingKnowledge
train knowledge@(EllipsoidKnowledge d eta' a' w') example (LInt trueY) = do
  let yhat = labelToInt $ getValFromWriter $ classify knowledge example
  tell $ toDiffList $ "Called train with:\n" ++ show knowledge ++
    ", the prediction " ++ show yhat
  if yhat == trueY
    then do
      tell $ toDiffList $ " was correct, so the result was:\n" ++ show knowledge  ++ "\n\n"
      return knowledge
    else do
      let ax = a' * example
      let xax = (transpose example * ax) ! (1, 1)
      let dDouble = fromInteger $ toInteger d
      let newW = w' + scaleMatrix (fromIntegral trueY / ((dDouble + 1) * sqrt xax)) (transpose ax)
      let newA = scaleMatrix eta' (a' - scaleMatrix (2 / ((dDouble + 1) * xax))  ax * transpose ax)
      let newKnowledge = EllipsoidKnowledge d eta' newA newW
      tell $ toDiffList $ " was wrong, so the result was:\n" ++ show newKnowledge  ++ "\n\n"
      return newKnowledge

train _ _ _ = Prelude.error "Train doesn't support these types"

-- Given an example and a knowledge, classifies the example
classify :: TrainingKnowledge -> Example -> Writer (DiffList Char) Label
classify knowledge@(EllipsoidKnowledge _ _ _ w') example =  do
  let res = LInt $ round (signum (w'  * example) ! (1, 1))
  tell $ toDiffList $ "Called classify with:\n" ++ show knowledge ++
    " and example: " ++ show example ++ ", prediction is:\n" ++ show res  ++ "\n\n"
  return res

-- Performs batch training on a batch of labeled examples
batch :: TrainingKnowledge -> [Example] -> [Label] -> Writer (DiffList Char) TrainingKnowledge
batch knowledge [] _ = do
    tell $ toDiffList "Called batch with no examples, nothing to be done\n"
    return knowledge
batch knowledge _ [] = do
    tell $ toDiffList "Called batch with no labels, nothing to be done\n"
    return knowledge
batch knowledge examples@(x:xs) labels@(y:ys) =
  if correctLength then do
      let res = getValFromWriter $ batch (getValFromWriter (train knowledge x y)) xs ys
      tell $ toDiffList $ "Called batch with:\n" ++ show knowledge ++
        "examples:\n " ++ showIndent examples ++ "\nand labels:\n" ++
        showIndent labels ++ "\nthe result was:\n"  ++ show res  ++ "\n\n"
      return res
      else do
        tell $ toDiffList "Number of examples and labels mismatch\n\n"
        errorReturn
  where
    correctLength = length examples == length labels
    errorReturn = Prelude.error "Examples length mismatch labels length\n\n"

-- A function that is used to calculate the training error of the classifier
error :: TrainingKnowledge -> Loss -> [Example] -> [Label] -> Writer (DiffList Char) Double
error knowledge loss xs ys = do
    let res = sum [loss curY_hat curY | (curY_hat, curY) <- zip y_hat ys] / fromIntegral (length ys)
    tell $ toDiffList $ "Called error with:\n" ++ show knowledge ++
      "examples:\n " ++ showIndent xs ++ "\nand labels:\n " ++ showIndent ys ++
      "the error rate was: "  ++ show res ++ "\n\n"
    return res
  where
    y_hat = map (getValFromWriter . classify knowledge) xs
