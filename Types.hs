{- |
Module: $Header$
Copyright: (c) Manuel Pacheco 2016
License: MIT

Maintainer: manuelalejandropm@gmail.com

Data types used to represent the problem

-}
module Types
( Sample(Sample)
, x
, y

, Hypothesis(Hypothesis)
, c
) where

data Sample a = Sample { x :: [ a ] , y :: a }
  deriving (Show)

data Hypothesis a = Hypothesis { c :: [a] }
  deriving (Show)
