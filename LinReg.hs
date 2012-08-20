module LinReg (

  lsr, bulkPDs

  ) where

import Poly
import Data.Packed.Matrix
import Numeric.LinearAlgebra.LAPACK
import Data.Monoid
import Control.Arrow

type Data = [(Double,Double)]

bulkPDs :: Int -> Data -> PartialDerivitives
bulkPDs ord dat = pard
  where
    poly = polynomial_of_order               ord
    dsqr = delta_squared_term                poly
    ssss = mconcat $ map (point_result dsqr) dat
    pard = partial_derivatives ord           ssss

point_result :: Expression -> (Double,Double) -> SimpleSum
point_result e (x,y) = normal (Env [("x", x), ("y", y)]) e

zipPds :: [ PartialDerivitives ] -> PartialDerivitives
zipPds = mconcat

lsr :: Int -> Data -> [Double]
lsr = undefined
