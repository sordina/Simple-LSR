{-# LANGUAGE GADTs #-}

module Poly where

-- Linear Regression Analysis using Least Squares Regression

import Data.Ord (comparing)
import Data.Function (on)
import Data.List
import Data.Monoid hiding (Sum, Product)
import Control.Monad

-- Data-Types

newtype Environment  = Env  [(String,Double)] deriving Show
newtype Environments = Envs [Environment]     deriving Show

data Expression where
  Number   ::  Double      -> Expression
  Variable ::  String      -> Expression
  Constant ::  String      -> Expression
  Sum      :: [Expression] -> Expression
  Product  :: [Expression] -> Expression

  deriving Show

newtype SimpleSum     = SS { unSS :: [ SimpleProduct ] } deriving Show
newtype SimpleProduct = SP { unSP :: [ SimpleTerm    ] } deriving Show

instance Monoid SimpleProduct where
  mempty      = SP []
  mappend l r = SP (unSP l ++ unSP r)

data SimpleTerm where
  SimpleNumber   :: Double -> SimpleTerm
  SimpleConstant :: String -> SimpleTerm

  deriving (Show, Eq, Ord)

newtype PartialDerivitives = PD { unPD :: [ SimpleSum ]      } deriving Show
newtype ConstantTerms      = CT { unCT :: [(String, Double)] } deriving Show

-- Display helpers for Simple Results

showSS :: SimpleSum -> String
showSS ss = intercalate " + " (map showSP $ unSS ss)

showSP :: SimpleProduct -> String
showSP sp = unwords (map showST $ unSP sp)

showST :: SimpleTerm -> String
showST (SimpleNumber n)   = show n
showST (SimpleConstant c) = c

-- Program

expressionOrder :: Expression -> Int
expressionOrder = length . distinct . variables

variables :: Expression -> [String]
variables = distinct . inner
  where
    inner :: Expression -> [String]
    inner (Variable s) = return s
    inner (Sum      s) = mconcat $ map inner s
    inner (Product  p) = mconcat $ map inner p
    inner _            = mempty

polynomial_of_order :: Int -> Expression
polynomial_of_order n = combined
  where
    combined  = Sum $ zipWith (\x y -> Product [x, y]) constants vars
    vars      =       map     (Product  . times (Variable "x")) [ 0  .. n]
    constants =       map     (Constant . return)               ['a' ..  ]

delta_squared_term :: Expression -> Expression
delta_squared_term e = Sum [Product [e,e], Product [Number _2, Variable "y", e]]

normal :: Environment -> Expression -> SimpleSum
normal _         (Number   n) = SS [ SP [ SimpleNumber n ]]
normal (Env env) (Variable v) = maybe (SS []) (\n -> SS [ SP [ SimpleNumber n ]]) (lookup v env)
normal _         (Constant c) = SS [ SP [ SimpleConstant c ]]
normal e         (Sum      s) = expression_sum     $ map (normal e) s
normal e         (Product  p) = expression_product $ map (normal e) p

expression_sum :: [SimpleSum] -> SimpleSum
expression_sum = SS . concatMap unSS

expression_product :: [SimpleSum] -> SimpleSum
expression_product = SS . cpm . map unSS

test1_data :: Expression
test1_data = polynomial_of_order 3

test2_data :: Expression
test2_data = delta_squared_term test1_data

test3_data :: SimpleSum
test3_data = normal (Env [("x", 10), ("y", 20)]) test2_data

test4_data :: PartialDerivitives
test4_data = partial_derivatives 3 test3_data

partial_derivatives :: Int -> SimpleSum -> PartialDerivitives
partial_derivatives order ss = PD $ map (flip partial_derivative ss) constants
  where
    constants = take (order+1) $ map return ['a'..]

partial_derivative :: String -> SimpleSum -> SimpleSum
partial_derivative s = SS
                     . groupLike
                     . map (multiply . remove1 s . addCount s)
                     . filter (relevant s)
                     . unSS

groupLike :: [ SimpleProduct ] -> [ SimpleProduct ]
groupLike = map merge
          . groupBy ((==) `on` extractConstants)
          . sortBy  (comparing extractConstants)
  where
    extractConstants :: SimpleProduct -> [ SimpleTerm ]
    extractConstants (SP s) = sort $ filter (not . isNumeric) s

    merge :: [SimpleProduct] -> SimpleProduct
    merge []      = SP []
    merge l@(x:_) = SP
                  $ SimpleNumber (sum (concatMap (map toNum . unSP) l))
                  : filter (not . isNumeric) (unSP x)
      where
        toNum :: SimpleTerm -> Double
        toNum (SimpleNumber   n) = n
        toNum (SimpleConstant _) = 0

relevant :: String -> SimpleProduct -> Bool
relevant s (SP sp) = SimpleConstant s `elem` sp

remove1 ::  String -> SimpleProduct -> SimpleProduct
remove1 s (SP p) = SP $ delete (SimpleConstant s) p

addCount :: String -> SimpleProduct -> SimpleProduct
addCount s (SP p) = SP $ SimpleNumber len : p
  where
    len = fromIntegral $ length $ filter (== SimpleConstant s) p

multiply :: SimpleProduct -> SimpleProduct
multiply (SP s) = let (ns,cs) = partition isNumeric s
                in SP $ SimpleNumber (product (map toNum ns)) : cs
  where
    toNum :: SimpleTerm -> Double
    toNum (SimpleNumber   n) = n
    toNum (SimpleConstant _) = 1

isNumeric :: SimpleTerm -> Bool
isNumeric (SimpleNumber   _) = True
isNumeric (SimpleConstant _) = False

solve_linear :: PartialDerivitives -> ConstantTerms
solve_linear = undefined

reduce :: Int -> SimpleSum -> ConstantTerms
reduce o s = solve_linear $ partial_derivatives o s

solve_for_order :: Int -> Environment -> ConstantTerms
solve_for_order o e = reduce o $ normal e (polynomial_of_order o)

-- Library

cp :: (Monad m, Monoid a) => m a -> m a -> m a
cp = liftM2 (<>)

cpm :: (Monad m, Monoid a) => [m a] -> m a
cpm = foldr cp (return mempty)

distinct :: (Ord a, Eq a) => [a] -> [a]
distinct = nub . sort

times :: b -> Int -> [b]
times = flip replicate

_2 :: Double
_2 = negate 2
