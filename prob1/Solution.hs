{- |
Module: $Header$
Copyright: (c) Manuel Pacheco 2016
License: MIT

Maintainer: manuelalejandropm@gmail.com

Functions to solve the problem 1

-}
module Solution
( veryClose
, addOnes
, theta
, cost
, descend
, gd
) where

import Data.List (foldl', unfoldr)
import Control.Monad (liftM2)
-- import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Backend.Cairo

import Types
import Constants (epsilon)

-- ASSISTING FUNCTIONS

-- Checks if two doubles are equal (epsilon difference is negligible)
veryClose :: Double -> Double -> Bool
veryClose v0 v1 = (<=epsilon) . abs $ v1 - v0

-- Prepends a constant coefficient to a list of samples
addOnes :: [Sample Double] -> [Sample Double]
addOnes = map $ liftM2 Sample ((1:) . x) y

-- Calculates the dot product between hypothesis and a sample
theta :: Hypothesis Double -> Sample Double -> Double
theta h s = sum $ zipWith (*) (c h) (x s)

-- Measures the quality of a hypothesis
cost :: Hypothesis Double -> [Sample Double] -> Double
cost h ss = cleaner $ foldl' adder seed ss
  where
    -- (Accumulated, No of elements)
    seed = (0, 0)
    adder (acc, elems) s = (acc + (theta h s - y s)**2, elems+1)
    cleaner (acc, elems) = acc / (2 * elems)

-- FOLD/UNFOLD CORE FUNCTIONS

-- Improves the quality of a hypothesis
descend :: Double -> Hypothesis Double -> [Sample Double] -> Hypothesis Double
descend alpha h ss = Hypothesis { c = result }
  where
    -- Fold through samples, accumulating how much each sample affects
    -- every component of the next hypothesis

    result = cleaner $ foldl' adder seed ss
    seed = (repeat 0, 0) -- accumulating load by components, no. of elements
    adder (acc, m) s = (zipWith (+) acc (calc s), m+1)
    cleaner (acc, m) = zipWith (\oj load -> oj - (alpha * load / m)) (c h) acc

    -- Inner fold simply modeled as a map
    -- the zipWih of adder does the rest of the job
    calc s = map (* (theta h s - y s)) (x s)

-- Generales a list that improves a hypothesis in each step
gd :: Double -> Hypothesis Double -> [Sample Double] -> [(Integer, Hypothesis Double, Double)]
gd alpha h ss = unfoldr generate seed
  where
    ss0 = addOnes ss
    miniseed = (0, h, cost h ss0) -- (Iteration, Hypothesis, Cost)
    seed = (miniseed, advance miniseed)

    advance (it, hyp, _) = (it', hyp', co')
      where
        it' = it + 1
        hyp' = descend alpha hyp ss0
        co' = cost hyp' ss0

    getCost (_, _, co) = co

    -- Generator function
    generate (first, second) = if veryClose (getCost first) (getCost second)
      then Nothing
      else Just (first, (second, advance second))
