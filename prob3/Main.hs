{- |
Module: $Header$
Copyright: (c) Manuel Pacheco 2016
License: MIT

Maintainer: manuelalejandropm@gmail.com

Example main to explain and verify the solution functions

-}

module Main (main) where

-- Original data type
data Filesystem a = File a | Directory a [Filesystem a]

-- Tracking data type
-- At each step a crumb must remember:
-- left side, right side and parent filesystems
data Crumb a = Crumb [Filesystem a] [Filesystem a] a

-- For usage in zipper
type Breadcrumbs a = [Crumb a]

-- Zipper type
type Zipper a = (Filesystem a, Breadcrumbs a)

-- Functions over zippers
goDown :: Zipper a -> Zipper a
goDown (File _, _) = error "No se puede bajar en un archivo"
goDown (Directory e (x:xs), cs) = (x, trace:cs)
  where
    left = []
    right = xs
    parent = e

    trace = Crumb left right parent

-- Focus the right sibling
goRight :: Zipper a -> Zipper a
goRight (_, []) = error "La raiz del filesystem no puede navegar a la derecha"
goRight (_, Crumb _ [] _ : _) = error "No hay mas cosas para navegar a la derecha"
goRight (foc, c:cs) = (r, trace:cs)
  where
    Crumb ls (r:rs) p = c

    left = ls ++ [foc]
    right = rs
    parent = p

    trace = Crumb left right parent

-- Focus the left sibling
goLeft :: Zipper a -> Zipper a
goLeft (_, []) = error "La raiz del filesystem no puede navegar a la izquierda"
goLeft (_, Crumb [] _ _ : _) = error "No hay mas cosas para navegar a la izquierda"
goLeft (foc, c:cs) = (last ls, trace:cs)
  where
    Crumb ls rs p = c

    left = init ls
    right = foc : rs
    parent = p

    trace = Crumb left right parent

-- Focus the parent element
goBack :: Zipper a -> Zipper a
goBack (_, []) = error "La raiz del filesystem no puede navegar hacia atras"
goBack (foc, c:cs) = (foc', cs)
  where
    Crumb ls rs p = c
    foc' = Directory p (ls ++ foc:rs)

-- Return to the root of the filesystem
tothetop :: Zipper a -> Zipper a
tothetop z@(_, []) = z
tothetop z = goBack z

-- Change the focus of the zipper
modify :: (Filesystem a -> Filesystem a) -> Zipper a -> Zipper a
modify f (foc, cs) = (f foc, cs)

-- Wrap a filesystem inside a zipper
focus :: Filesystem a -> Zipper a
focus foc = (foc, [])

-- Unwrap a filesystem inside a zipper
defocus :: Zipper a -> Filesystem a
defocus (foc, _) = foc

main :: IO ()
main = do
  return ()
