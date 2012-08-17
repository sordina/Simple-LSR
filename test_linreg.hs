{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}

import System.Random
import Data.Monoid
import Control.Monad
import Data.List
import Control.Arrow
import Data.Tree
import Text.Groom

range :: (Double, Double)
range = (-10,10)

list :: [Double]
list = [0,10..300]

xvals :: [Double]
xvals = [0..]

points :: IO [(Double,Double)]
points = fmap (zip xvals . zipWith (+) list . randomRs range) newStdGen

main :: IO ()
main = points >>= print

-- Library

type Data       = [(Double,Double)]
type Equations  = [[Double]]
type Solution   = [Double]
type Polynomial = [Double]

lsr :: Int -> Data -> Solution
lsr n = solveLinear . mkSystem n

mkSystem :: Int -> Data -> Equations
mkSystem n = contract . expand n

solveLinear :: Equations -> Solution
solveLinear = undefined

derivitive :: Polynomial -> Polynomial
derivitive = tail . zipWith (*) [0..]

expand = undefined

contract = undefined

term :: Int -> Double -> [Double]
term a n = zipWith (^) (replicate a n) [0 :: Int ..]

term2 :: [Double] -> [Double]
term2 = undefined

cp :: (Monad m, Monoid a) => m a -> m a -> m a
cp = liftM2 (<>)

newtype Foo x = Foo {unFoo :: x}

instance (Monad m, Monoid a) => Monoid (Foo (m a)) where
  mappend (Foo l) (Foo r) = Foo (cp l r)
  mempty                  = Foo $ return $ mempty

cpm :: (Monad m, Monoid a) => [m a] -> m a
cpm = unFoo . mconcat . map Foo

test1 = cp l l where l = [words "x x a", words "x b", words "c"]
test2 = delta_squared_term (poly 2)
test3 = putStrLn $ groom test2
test4 = partial_derivative "a" test2
test5 = map (map (mconcat . mconcat . intersperse ["."] . map power . group . sort)) (augmented_matrix 3)
test6 = mapM_ (putStrLn . ("0 = " ++) . mconcat . intersperse " + ") test5

data Expression where
  Number   ::  Double      -> Expression
  Variable ::  String      -> Expression
  Constant ::  String      -> Expression
  Sum      :: [Expression] -> Expression
  Multiple ::  Expression  -> Expression -> Expression

expression_order :: Expression -> Bool
expression_order = undefined

power :: [String] -> [String]
power l | len > 1   = [head l ++ "^" ++ show len]
        | otherwise = l
  where len = length l

-- TODO: Better to create an intermediate literal free datatype here that has a
-- Show instance like "ax^2+bx+c".
--
poly :: Int -> [[String]]
poly n = combined
  where
    combined  = zipWith (<>) variables constants
    variables = map (flip replicate "x") [0..n]
    constants = (map (return . return) ['a'..])

delta_squared_term :: [[String]] -> [[String]]
delta_squared_term p = cp p p <> cpm [[["-2"]], [["y"]], p]

partial_derivative :: String -> [[String]] -> [[String]]
partial_derivative v = map (remove_one . times_length) . filter (elem v)
  where
    remove_one   = delete v
    times_length = uncurry (:) . ((show . length . filter (== v)) &&& id)

augmented_matrix :: Int -> [[[String]]]
augmented_matrix n = zipWith partial_derivative variables (repeat $ delta_squared_term $ poly n)
  where
    variables = map return $ take (n+1) ['a'..]

-- mconcat = foldr mappend mempty
-- foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
--- Countdown

{-
splits    = uncurry zip . (inits &&& tails)
choices   = unfoldForest step . splits
step pair = map ($pair) ops &&& undefined
ops       = undefined
-}

-- unfoldTree   :: (b -> (a, [b])) -> b   -> Tree   a
-- unfoldForest :: (b -> (a, [b])) -> [b] -> Forest a
-- data Tree   a = Node {rootLabel :: a, subForest :: Forest a}
-- type Forest a = [Tree a]
