import Test.HUnit

import Math.Spline.Knots
import Math.Spline.BSpline
import Math.Spline.NurbsSurface

import Diagrams.ThreeD.Shapes

kLinear = mkKnots [0,0,1,1] -- degree 1

kCircle = mkKnots [0,0,0,0.25,0.25,0.5,0.5,0.75,0.75,1,1,1]

knotsTest = test [
  "findSpan" ~: [
     "linear" ~: ["0" ~: Just 1 ~=? findSpan kLinear 0
                 ,"0.5" ~: Just 1 ~=? findSpan kLinear 0.5
                 ,"1" ~: Just 1 ~=? findSpan kLinear 1],
     "circle" ~: ["0" ~: Just 2 ~=? findSpan kCircle 0,
                  "0.1" ~: Just 2 ~=? findSpan kCircle 0.1,
                  "0.25" ~: Just 4 ~=? findSpan kCircle 0.25,
                  "0.3" ~: Just 4 ~=? findSpan kCircle 0.3,
                  "0.5" ~: Just 6 ~=? findSpan kCircle 0.5,
                  "0.75" ~: Just 8 ~=? findSpan kCircle 0.75,
                  "1" ~: Just 8 ~=? findSpan kCircle 1]],
  "basisFuns" ~: [
    "linear" ~: ["0" ~: [1,0] ~=? basisFuns' 1 kLinear 0,
                 "0.25" ~: [0.75, 0.25] ~=? basisFuns' 1 kLinear 0.25,
                 "0.5" ~: [0.5,0.5] ~=? basisFuns' 1 kLinear 0.5,
                 "1" ~: [0,1] ~=? basisFuns' 1 kLinear 1],
    "circle" ~: ["0" ~: [1,0,0] ~=? basisFuns' 2 kCircle 0,
                 "0.1" ~: expectedU2 (0.1/0.25) ~=? basisFuns' 2 kCircle 0.1,
                 "0.24" ~: expectedU2 (0.24/0.25) ~=? basisFuns' 2 kCircle 0.24,
                 "0.25" ~: expectedU2 0 ~=? basisFuns' 2 kCircle 0.25
                   ]]]

nurbsSurface = "evalSurface" ~: [
  "cylinder" ~: ["0,0" ~: (1,0,0) ~=? evalSurface cyl 0 0,
                 "1,0" ~: (1,0,1) ~=? evalSurface cyl 1 0,
                 "0,0.25" ~: (0,1,0) ~=? evalSurface cyl 0 0.25]]

u2 n x = (1-x)**(2-n) * x**n

expectedU2 x = [u2 0 x, 2 * (u2 1 x), u2 2 x]