{-# LANGUAGE GADTs #-}

import Data.List
import Data.Monoid
import Control.Monad

-- Data:

data Expression where
  Number   ::  Double      -> Expression
  Variable ::  String      -> Expression
  Constant ::  String      -> Expression
  Sum      :: [Expression] -> Expression
  Multiple ::  Expression  -> Expression -> Expression

  deriving (Show)

expressionOrder :: Expression -> Int
expressionOrder = length . distinct . variables

variables :: Expression -> [String]
variables = undefined

distinct :: (Ord a, Eq a) => [a] -> [a]
distinct = nub . sort

-- Library:

cp :: (Monad m, Monoid a) => m a -> m a -> m a
cp = liftM2 (<>)

cpm :: (Monad m, Monoid a) => [m a] -> m a
cpm = foldr cp (return mempty)
