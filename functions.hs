import Data.List (foldl')
-- import Data.Functor
-- import Data.Monoid
-- import Data.Foldable ( foldMap )
-- import Data.Tree
-- import Graphics.Rendering.Chart.Easy
-- import Graphics.Rendering.Chart.Backend.Cairo

data Sample a = Sample { x :: [ a ] , y :: a }
  deriving (Show)

data Hypothesis a = Hypothesis { c :: [a] }
  deriving (Show)

alpha :: Double
alpha = 0.03

epsilon :: Double
epsilon = 0.0000001

guess :: Hypothesis Double
guess = Hypothesis { c = [0.0, 0.0, 0.0] }

-- Functions to implement
veryClose :: Double -> Double -> Bool
veryClose v0 v1 = (<=epsilon) . abs $ v1 - v0

addOnes :: [Sample Double] -> [Sample Double]
addOnes = map addOne
  where
    addOne e = e { x = 1.0 : x e }

theta :: Hypothesis Double -> Sample Double -> Double
theta h s = sum $ zipWith (*) (c h) (x s)

cost :: Hypothesis Double -> [Sample Double] -> Double
cost h ss = cleaner $ foldl' adder seed ss
  where
    -- (Accumulated, No of elements)
    seed = (0, 0)
    adder (acc, elems) s = (acc + (theta h s - y s)**2, elems+1)
    cleaner (acc, elems) = acc / (2 * elems)

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

main :: IO ()
main = return ()
