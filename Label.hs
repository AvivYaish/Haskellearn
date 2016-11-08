-- This module represents the possible labels that an example can have.
-- We chose Double and Int because these are the most basic labels possible -
-- A double can be used for most regression problems (up to a specific precision or scale).
-- An int can be used for more classification problems.
-- We've implemented this module with extensibility in mind.
-- We implemented the LabelType data in order to be able to check the type of a
-- specific label - is it a Double, or an Int?
-- This is important for the KNN leaner that can either function as a regression
-- learner or a classifier.

module Label(Label(..), LabelType(..), labelToDouble, labelToInt) where

-- LabelType data type represents the type of the labels that can be returned by
-- a learner
data LabelType = DoubleType | IntType deriving Show

-- Label datatype represents the value returned by a learner
data Label = LDouble Double | LInt Int deriving Show

instance Ord Label where
  (LDouble x) `compare` (LDouble y) = x `compare` y
  (LInt x) `compare` (LInt y) = x `compare` y
  _ `compare` _ = Prelude.error "Can't compare these types"

instance Eq Label where
  (==) (LDouble x) (LDouble y) = x == y
  (==) (LInt x) (LInt y) = x == y
  (==) _ _ = Prelude.error "Can't compare these types"
  (/=) (LDouble x) (LDouble y) = x /= y
  (/=) (LInt x) (LInt y) = x /= y
  (/=) _ _ =  Prelude.error "Can't compare these types"

-- A function that get the double value out of a label when it hold a double value.
labelToDouble :: Label -> Double
labelToDouble (LDouble a) = a
labelToDouble _ = Prelude.error "Label isn't a double"

-- A function that get the double value out of a label when it hold a int value.
labelToInt :: Label ->  Int
labelToInt (LInt a) = a
labelToInt _ = Prelude.error "Label isn't an int"
