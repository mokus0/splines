#!/usr/bin/env runhaskell
module Main where

import Test.Framework (defaultMain, testGroup)

import Test.Knots (knotsTests)
import Test.Reference (referenceBSplineTests)

main = defaultMain 
    [ testGroup "Math.Spline.BSpline.Reference" referenceBSplineTests
    , testGroup "Math.Spline.Knots"             knotsTests
    ]