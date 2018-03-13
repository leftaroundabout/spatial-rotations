-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2018
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE OverloadedLists, TypeFamilies, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import qualified Text.Show.Pragmatic as SP
import Math.Rotations.Class

import Data.Manifold

import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC
import Test.Tasty.QuickCheck ((==>))


main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
 [ testGroup "Rotation matrices"
    [ testCase "180° x-axis" $ rotmatrixForAxis xAxis (rad π)
                      @?≈ [ [1,0,0], [0,-1,0], [0,0,-1] ]
    , testCase " 90° x-axis" $ rotmatrixForAxis xAxis (rad π'₂)
                      @?≈ [ [1,0,0], [0,0,-1], [0,1,0] ]
    , testCase "-90° x-axis" $ rotmatrixForAxis xAxis (rad $ -π'₂)
                      @?≈ [ [1,0,0], [0,0,1], [0,-1,0] ]
    , testCase " 45° x-axis" $ rotmatrixForAxis xAxis (rad π'₄)
                      @?≈ [ [1,0,0], [0,sqrt 2/2,-sqrt 2/2], [0,sqrt 2/2,sqrt 2/2] ]
    , testCase "180° y-axis" $ rotmatrixForAxis yAxis (rad π)
                      @?≈ [ [-1,0,0], [0,1,0], [0,0,-1] ]
    , testCase " 90° y-axis" $ rotmatrixForAxis yAxis (rad π'₂)
                      @?≈ [ [0,0,1], [0,1,0], [-1,0,0] ]
    , testCase "-90° y-axis" $ rotmatrixForAxis yAxis (rad $ -π'₂)
                      @?≈ [ [0,0,-1], [0,1,0], [1,0,0] ]
    , testCase " 45° y-axis" $ rotmatrixForAxis yAxis (rad π'₄)
                      @?≈ [ [sqrt 2/2,0,sqrt 2/2], [0,1,0], [-sqrt 2/2,0,sqrt 2/2] ]
    , testCase "180° z-axis" $ rotmatrixForAxis zAxis (rad π)
                      @?≈ [ [-1,0,0], [0,-1,0], [0,0,1] ]
    , testCase " 90° z-axis" $ rotmatrixForAxis zAxis (rad π'₂)
                      @?≈ [ [0,-1,0], [1,0,0], [0,0,1] ]
    , testCase "-90° z-axis" $ rotmatrixForAxis zAxis (rad $ -π'₂)
                      @?≈ [ [0,1,0], [-1,0,0], [0,0,1] ]
    , testCase " 45° z-axis" $ rotmatrixForAxis zAxis (rad π'₄)
                      @?≈ [ [sqrt 2/2,-sqrt 2/2,0], [sqrt 2/2,sqrt 2/2,0], [0,0,1] ]
    ]
 , testGroup "Euler angles"
    [ testCase "180° x-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis xAxis (rad π))
                      @?≈ [ π³₄,  π ,-π'₄]
    , testCase " 90° x-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis xAxis (rad π'₂))
                      @?≈ [ π'₂, π'₂,-π'₂]
    , testCase "-90° x-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis xAxis (rad $ -π'₂))
                      @?≈ [-π'₂, π'₂, π'₂]
    , testCase " 45° x-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis xAxis (rad π'₄))
                      @?≈ [ π'₂, π'₄,-π'₂]
    , testCase "180° y-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis yAxis (rad π))
                      @?≈ [ π'₄, π  , π'₄]
    , testCase " 90° y-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis yAxis (rad π'₂))
                      @?≈ [ 0  , π'₂, 0  ]
    , testCase "-90° y-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis yAxis (rad $ -π'₂))
                      @?≈ [-π  , π'₂, π  ]
    , testCase " 45° y-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis yAxis (rad π'₄))
                      @?≈ [ 0  , π'₄, 0  ]
    , testCase "180° z-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis zAxis (rad π))
                      @?≈ [ π  ,  0 , 0  ]
    , testCase " 90° z-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis zAxis (rad π'₂))
                      @?≈ [ π  ,  0 ,-π'₂]
    , testCase "-90° z-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis zAxis (rad $ -π'₂))
                      @?≈ [ π  ,  0 , π'₂]
    , testCase " 45° z-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis zAxis (rad π'₄))
                      @?≈ [ π  , 0  ,-π³₄]
    , QC.testProperty "Arbitrary rotation-matrix reconstruction"
         $ \ax α -> let mat = rotmatrixForAxis ax α
                    in rotmatrixForEulerAnglesZYZ (eulerAnglesZYZForMatrix mat) ≈ mat
    ]
 , testGroup "Concrete 180° rotations"
    [ testCase "x around x" $ (180° xAxis) (π'₂ ⦺ 0  ) @?≈ π'₂ ⦺  0
    , testCase "..full rot" $ (180° xAxis)                (π'₂ ⦺  0  )@?≈ π'₂ ⦺ 0
    , testCase "x around y" $ (180° yAxis) (π'₂ ⦺ 0  ) @?≈ π'₂ ⦺  π
    , testCase "..full rot" $ (180° yAxis)                (π'₂ ⦺  π  )@?≈ π'₂ ⦺ 0
    , testCase "x around z" $ (180° zAxis) (π'₂ ⦺ 0  ) @?≈ π'₂ ⦺  π
    , testCase "..full rot" $ (180° zAxis)                (π'₂ ⦺  π  )@?≈ π'₂ ⦺ 0
    , testCase "y around x" $ (180° xAxis) (π'₂ ⦺ π'₂) @?≈ π'₂ ⦺ -π'₂
    , testCase "..full rot" $ (180° xAxis)                (π'₂ ⦺ -π'₂)@?≈ π'₂ ⦺ π'₂
    , testCase "y around y" $ (180° yAxis) (π'₂ ⦺ π'₂) @?≈ π'₂ ⦺  π'₂
    , testCase "..full rot" $ (180° yAxis)                (π'₂ ⦺  π'₂)@?≈ π'₂ ⦺ π'₂
    , testCase "y around z" $ (180° zAxis) (π'₂ ⦺ π'₂) @?≈ π'₂ ⦺ -π'₂
    , testCase "..full rot" $ (180° zAxis)                (π'₂ ⦺ -π'₂)@?≈ π'₂ ⦺ π'₂
    , testCase "z around x" $ (180° xAxis) (0   ⦺ 0  ) @?≈ π   ⦺  0
    , testCase "..full rot" $ (180° xAxis)                (π   ⦺  0  )@?≈ 0 ⦺ 0
    , testCase "z around y" $ (180° yAxis) (0   ⦺ 0  ) @?≈ π   ⦺  0
    , testCase "..full rot" $ (180° yAxis)                (π   ⦺  0  )@?≈ 0 ⦺ 0
    , testCase "z around z" $ (180° zAxis) (0   ⦺ 0  ) @?≈ 0   ⦺  0
    , testCase "..full rot" $ (180° zAxis)                (0   ⦺  0  )@?≈ 0 ⦺ 0
    ]
 , testGroup "Concrete 90° rotations"
    [ testCase "x around x" $ (90° xAxis) (π'₂ ⦺ 0  ) @?≈ π'₂ ⦺ 0
    , testCase "x around y" $ (90° yAxis) (π'₂ ⦺ 0  ) @?≈ π   ⦺ 0
    , testCase "x around z" $ (90° zAxis) (π'₂ ⦺ 0  ) @?≈ π'₂ ⦺ π'₂
    , testCase "y around x" $ (90° xAxis) (π'₂ ⦺ π'₂) @?≈ 0   ⦺ 0
    , testCase "y around y" $ (90° yAxis) (π'₂ ⦺ π'₂) @?≈ π'₂ ⦺ π'₂
    , testCase "y around z" $ (90° zAxis) (π'₂ ⦺ π'₂) @?≈ π'₂ ⦺ π
    , testCase "z around x" $ (90° xAxis) (0   ⦺ 0  ) @?≈ π'₂ ⦺ (-π'₂)
    , testCase "z around y" $ (90° yAxis) (0   ⦺ 0  ) @?≈ π'₂ ⦺ 0
    , testCase "z around z" $ (90° zAxis) (0   ⦺ 0  ) @?≈ 0   ⦺ π'₂
    
    , testCase "around y, over x" $ (90° yAxis) (π'₄ ⦺  0  ) @?≈ π³₄ ⦺ 0
    , testCase "around z, over x" $ (90° zAxis) (π'₂ ⦺ -π'₄) @?≈ π'₂ ⦺ π'₄
    , testCase "around x, over y" $ (90° xAxis) (π³₄ ⦺  π'₂) @?≈ π'₄ ⦺ π'₂
    , testCase "around z, over y" $ (90° zAxis) (π'₂ ⦺  π'₄) @?≈ π'₂ ⦺ π³₄
    , testCase "around x, over z" $ (90° xAxis) (π'₄ ⦺  π'₂) @?≈ π'₄ ⦺ -π'₂
    , testCase "around y, over z" $ (90° yAxis) (π'₄ ⦺  π  ) @?≈ π'₄ ⦺ 0
    ]
 , testGroup "Concrete 45° rotations"
    [ testCase "x around x" $ (45° xAxis) (π'₂ ⦺ 0  ) @?≈ π'₂ ⦺  0
    , testCase "x around y" $ (45° yAxis) (π'₂ ⦺ 0  ) @?≈ π³₄ ⦺  0
    , testCase "x around z" $ (45° zAxis) (π'₂ ⦺ 0  ) @?≈ π'₂ ⦺  π'₄
    , testCase "y around x" $ (45° xAxis) (π'₂ ⦺ π'₂) @?≈ π'₄ ⦺  π'₂
    , testCase "y around y" $ (45° yAxis) (π'₂ ⦺ π'₂) @?≈ π'₂ ⦺  π'₂
    , testCase "y around z" $ (45° zAxis) (π'₂ ⦺ π'₂) @?≈ π'₂ ⦺  π³₄
    , testCase "z around x" $ (45° xAxis) (0   ⦺ 0  ) @?≈ π'₄ ⦺ -π'₂
    , testCase "z around y" $ (45° yAxis) (0   ⦺ 0  ) @?≈ π'₄ ⦺  0
    , testCase "z around z" $ (45° zAxis) (0   ⦺ 0  ) @?≈ 0   ⦺  π'₂
    ]
 , testGroup "Reversability"
    [ QC.testProperty "Arbitrary axis, angles and points"
           $ \ax ψ p -> rotateAboutThenUndo ax ψ p ≈ p
    ]
 ]
 where π = pi
       π'₂ = pi/2
       π'₄ = pi/4
       π³₄ = 3*pi/4



infix 4 ≈
class AEq e where
  (≈) :: e -> e -> Bool

instance AEq Double where
  x ≈ y = abs (x-y) < 1e-9
instance AEq S¹ where
  S¹Polar φ ≈ S¹Polar ϕ
   | φ > pi/2, ϕ < -pi/2  = S¹Polar (φ - 2*pi) ≈ S¹Polar ϕ
   | ϕ > pi/2, φ < -pi/2  = S¹Polar φ ≈ S¹Polar (ϕ - 2*pi)
   | otherwise            = φ ≈ ϕ
instance AEq S² where
  S²Polar θ φ ≈ S²Polar ϑ ϕ
   | φ > pi/2, ϕ < -pi/2  = S²Polar θ (φ - 2*pi) ≈ S²Polar ϑ ϕ
   | ϕ > pi/2, φ < -pi/2  = S²Polar θ φ ≈ S²Polar ϑ (ϕ - 2*pi)
   | otherwise            = θ ≈ ϑ && abs (φ - ϕ) * sin θ < 1e-9
instance AEq a => AEq [a] where
  [] ≈ [] = True
  x:xs ≈ y:ys = x ≈ y && xs ≈ ys
  _ ≈ _ = False

                  
infix 1 @?≈       
(@?≈) :: (AEq e, Show e) => e -> e -> Assertion
a@?≈b
 | a≈b        = return ()
 | otherwise  = assertFailure $ "Expected "++show b++", but got "++show a

rad :: Double -> S¹
rad = S¹Polar

infix 5 ⦺
(⦺) :: Double -> Double -> S²
(⦺) = S²Polar

rotateAboutThenUndo :: ℝP² -> S¹ -> S² -> S²
rotateAboutThenUndo ax ψ@(S¹Polar w) p = rotateAbout ax (S¹Polar $ -w) $ rotateAbout ax ψ p
