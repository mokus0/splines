module Tabulate where

import Text.Printf
import Data.VectorSpace

-- quick and dirty "tabulation" function - takes a function and a range and
-- prints a table of values of the function to the console in a format suitable
-- for copying and pasting into Excel or Grapher.app
tabulate :: Int -> (Double -> [Double]) -> Double -> Double -> IO ()
tabulate n f x0 x1 = sequence_
    [ doubleRow (x : f x)
    | i <- [1..n]
    , let x = lerp x0 x1 (fromIntegral (i - 1) / fromIntegral (n - 1))
    ]

doubleRow :: [Double] -> IO ()
doubleRow = loop False
    where
        loop  sep    []     = printf "\n"
        loop False (x:xs)   = printf   "%g" x >> loop True xs
        loop True  (x:xs)   = printf "\t%g" x >> loop True xs
