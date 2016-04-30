{- |
Module: $Header$
Copyright: (c) Manuel Pacheco 2016
License: MIT

Maintainer: manuelalejandropm@gmail.com

Example main to explain and verify the solution functions

-}
module Main (main) where

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Types (Sample(Sample), x, y)
import Constants (alpha, guess)
import Samples (training)
import Solution (gd, theta)

sample :: Sample Double
sample = Sample { x = [1.0, -0.44127, -0.22368], y = undefined }

main :: IO ()
main = do
  putStrLn "Imprimiendo los 3 primeros resultados..."
  mapM_ print . take 3 $ gd alpha guess training
  putStr "\n"

  let
    results = gd alpha guess training
    lastResult = last results
    (_, vector, _) = lastResult

  putStrLn "Imprimiendo el ultimo resultado..."
  print lastResult
  putStr "\n"

  print $ "El valor v associado al vector " ++ show (x sample) ++ " es " ++ show (theta vector sample)
  putStr "\n"

  -- Code to generate the image file
  -- can be removed to discard Graphics imports
  toFile def "works.png" $ do
    layout_title .= "Cost over iterations"
    plot (line "am" . (:[]) . map (\(x,_,y) -> (x,y)) $ results)
  return ()
