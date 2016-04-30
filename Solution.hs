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
addOnes = map addOne
  where
    addOne e = e { x = 1.0 : x e }

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
descend alpha h ss = Hypothesis { c = c' }
  where
    -- Recursion to calculate all coefficients
    c' = cleaner $ foldl' adder seed (c h)
    seed = ([], 0) -- (coefficients, position)
    adder (ojs, j) oj = (calc oj j : ojs, j+1)
    cleaner (ojs, _) = reverse ojs

    -- Recursion to calculate a coefficient at position j
    calc oj j = oj - cleaner2 (foldl' (adder2 j) seed2 ss)
    seed2 = (0, 0) -- (product differences, No of elements)
    adder2 j (pdiffs, elems) s = (pdiffs + (theta h s - y s) * (x s !! j), elems + 1)
    cleaner2 (pdiffs, elems) = pdiffs * alpha / elems

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
