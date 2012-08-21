module Test where

import Poly
import LinReg
import Data.List
import Text.Groom
import Control.Arrow
import System.Random

-- Main test

main :: IO ()
main = test6

-- Data Generators

annealedData :: [Double] -> IO [(Double, Double)]
annealedData constants = do
  gen <- newStdGen
  return $ zipWith f (randomRs (0,1) gen) (functionData constants)

  where f a (x,y) = (x, a+y-0.5)

functionData :: [Double] -> [(Double, Double)]
functionData constants = map (id &&& f) [0..]
  where
    f x = sum $ zipWith (*) (g x) constants
    g x = zipWith (**) (repeat x) [0..]

-- Show Functions

showSS :: SimpleSum -> String
showSS ss = intercalate " + " (map showSP $ unSS ss)

showSP :: SimpleProduct -> String
showSP sp = unwords (map showST $ unSP sp)

showST :: SimpleTerm -> String
showST (SimpleNumber n)   = show n
showST (SimpleConstant c) = c

-- Test Functions

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

test5 :: IO ()
test5 = do
  test5_data <- fmap (bulkPDs 4 . take 10) (annealedData [1,2,3,4,5,6])
  mapM_ (putStrLn . showSS) (unPD test5_data)

test6 :: IO ()
test6 = do
  randomData <- annealedData [0,2,0,0,-8]

  let order      = 5
      points     = take 100 randomData
      linear     = bulkPDs (order - 1) points

  putStrLn ""
  putStrLn "Order:"
  print order
  putStrLn ""

  putStrLn "Data:"
  putStrLn $ groom $ points
  putStrLn ""

  putStrLn "Linear System of Derivatives:"
  mapM_ (putStrLn . showSS) (unPD linear)
  putStrLn ""

  putStrLn "Solved System:"
  mapM_ print $ lsr order points
  putStrLn ""
