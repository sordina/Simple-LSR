module LinReg (

  lsr, bulkPDs

  ) where

import Poly
import Data.List
import Data.Monoid
import Control.Arrow
import Data.Packed.Matrix
import Numeric.LinearAlgebra.LAPACK

type Data = [(Double,Double)]

-- Data Acquisition - Very unsafe

getData :: PartialDerivitives -> ([[Double]], [Double])
getData = (map tail &&& map (negate . head)) . fun
  where
    fun                         = map (map (getNums . unSP) . unSS) . unPD
    getNums                     = foldl' getNum 1
    getNum x (SimpleNumber   n) = n * x
    getNum x (SimpleConstant _) = x

-- Concat Partialderivitives over data-points

bulkPDs :: Int -> Data -> PartialDerivitives
bulkPDs ord dat = pard
  where
    poly = polynomial_of_order               ord
    dsqr = delta_squared_term                poly
    ssss = mconcat $ map (point_result dsqr) dat
    pard = partial_derivatives ord           ssss

point_result :: Expression -> (Double,Double) -> SimpleSum
point_result e (x,y) = normal (Env [("x", x), ("y", y)]) e

lsr :: Int -> Data -> [Double]
lsr order points = concat $ toLists $ linearSolveR coefs values
  where
    linear     = bulkPDs (order - 1) points
    (lhs, rhs) = getData linear
    coefs      = (order >< order) (concat lhs)
    values     = (order >< 1    )  rhs
