-- This module houses the loss functions used in our various learners.
-- More loss functions can be added to it easily.

module Loss(binaryLoss, quadriaticLoss, Loss) where

import Label

-- The Loss type represents a loss function
type Loss = Label -> Label -> Double

-- binaryLoss simply checks if the labels are identitcal or not
binaryLoss :: Loss
binaryLoss x y = if x /= y then 1 else 0

-- quadriaticLoss computes the quadriatic loss between two LDouble labels
quadriaticLoss :: Loss
quadriaticLoss (LDouble x) (LDouble y) = (x - y) ** 2
quadriaticLoss _ _ = Prelude.error "Only doubles are supported"
