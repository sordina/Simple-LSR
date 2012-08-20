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


-- Playing with HMatrix stuff

solve_linear :: PartialDerivitives -> ConstantTerms
solve_linear = undefined

reduce :: Int -> SimpleSum -> ConstantTerms
reduce o s = solve_linear $ partial_derivatives o s

solve_for_order :: Int -> Environment -> ConstantTerms
solve_for_order o e = reduce o $ normal e (polynomial_of_order o)

coefs :: Matrix Double
coefs = (4><4) [ 1, 2, 3, 3
               , 4, 5, 6, 2
               , 4, 5, 6, 2
               , 7, 8, 9, 1]

vals :: Matrix Double
vals = (4><1) [ 1, 2, 3, 4 ]

answer :: Matrix Double
answer = linearSolveLSR coefs vals
