
{- |
Module: $Header$
Copyright: (c) Manuel Pacheco 2016
License: MIT

Maintainer: manuelalejandropm@gmail.com

Example main to explain and verify the solution functions

-}

{-# LANGUAGE DeriveFoldable #-}

module Main (main) where

import Data.Foldable hiding (mapM_)
import Data.Monoid

-- Max newtype
newtype Max a = Max { getMax :: Maybe a }
  deriving (Show, Eq)

-- Monoid Instance for Max
instance (Ord a) => Monoid (Max a) where
  mempty = Max Nothing

  Max Nothing `mappend` Max Nothing = Max Nothing
  Max Nothing `mappend` y = y
  x `mappend` Max Nothing = x

  Max (Just x) `mappend` Max (Just y) = Max . Just $ max x y

-- Examples information
type Something = Int
data Node a = Node a [Node a]
  deriving (Eq, Show, Foldable)
-- Foldable is derived, but easily implementable as
-- instance Foldable Node where
  -- foldMap f (Node e ns) = f e `mappend` foldMap (foldMap f) ns

main :: IO ()
main = do
  -- Examples found on the assignment
  let e1 = foldMap (Max . Just) [] :: Max Something
  let e2 = foldMap (Max . Just) ["foo", "bar", "baz"]
  let e3 = foldMap (Max . Just) (Node 6 [Node 42 [], Node 7 [] ])
  let e4 = foldMap (Max . Just) (Node [] []) :: Max [Something]

  putStrLn "Resultados de los ejemplos del enunciado..."
  print e1
  print e2
  print e3
  print e4
