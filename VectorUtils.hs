-- Various utilities used throughout our project.

module VectorUtils(distanceFromVector, lNorm, Norm) where

-- Norm is a type that represents a norm function
type Norm = [Double] -> Double

-- A function that given a norm calculates the distance of a list of vectors from a specific vector.
distanceFromVector :: Norm -> [[Double]] -> [Double] -> [Double]
distanceFromVector norm x toClassify = map (norm . zipWith (-) toClassify) x

-- A function that is used to calculate the l norm of a given vector x
lNorm :: Double -> [Double] -> Double
lNorm n x = sum (map (** n) x) ** (1 / n)
