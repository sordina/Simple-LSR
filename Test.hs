module Test where

import Poly
import Data.List
import Text.Groom

showSS :: SimpleSum -> String
showSS ss = intercalate " + " (map showSP $ unSS ss)

showSP :: SimpleProduct -> String
showSP sp = unwords (map showST $ unSP sp)

showST :: SimpleTerm -> String
showST (SimpleNumber n)   = show n
showST (SimpleConstant c) = c

test1 :: IO ()
test1 = putStrLn $ groom test1_data

test1_data :: Expression
test1_data = polynomial_of_order 3

test2 :: IO ()
test2 = putStrLn $ groom test2_data

test2_data :: Expression
test2_data = delta_squared_term test1_data

test3 :: IO ()
test3 = putStrLn $ groom test3_data

test3_inline :: IO ()
test3_inline = putStrLn $ showSS test3_data

test3_data :: SimpleSum
test3_data = normal (Env [("x", 10), ("y", 20)]) test2_data

test4_data :: PartialDerivitives
test4_data = partial_derivatives 3 test3_data

test4 :: IO ()
test4 = putStrLn $ groom test4_data

test4_inline :: IO ()
test4_inline = mapM_ (putStrLn . showSS) (unPD test4_data)
