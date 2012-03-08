#!/usr/bin/env runhaskell
module Main where

import Test.Framework (defaultMain, testGroup)

import Tests.BSpline.Reference (referenceBSplineTests)
import Tests.BSpline (bSplineTests)
import Tests.Knots (knotsTests)

main = defaultMain 
    [ testGroup "Math.Spline.BSpline.Reference" referenceBSplineTests
    , testGroup "Math.Spline.BSpline"           bSplineTests
    , testGroup "Math.Spline.Knots"             knotsTests
    ]