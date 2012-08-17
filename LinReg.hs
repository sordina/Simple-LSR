module LinReg (

  lsr

  ) where

import Poly

lsr :: Int -> [(Double,Double)] -> [Double]
lsr = undefined

solve_linear :: PartialDerivitives -> ConstantTerms
solve_linear = undefined

reduce :: Int -> SimpleSum -> ConstantTerms
reduce o s = solve_linear $ partial_derivatives o s

solve_for_order :: Int -> Environment -> ConstantTerms
solve_for_order o e = reduce o $ normal e (polynomial_of_order o)
