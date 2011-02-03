#!/usr/bin/env runhaskell
module Main where

import Test.Framework (defaultMain, testGroup)

import Test.Knots (knotsTests)

main = defaultMain 
    [ testGroup "Math.Spline.Knots" knotsTests
    ]