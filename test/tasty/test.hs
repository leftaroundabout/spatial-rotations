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
    [ testCase "180° x-axis" $ rotmatrixForAxis xAxis (S¹ π)
                      @?≈ [ [1,0,0], [0,-1,0], [0,0,-1] ]
    , testCase " 90° x-axis" $ rotmatrixForAxis xAxis (S¹ π'₂)
                      @?≈ [ [1,0,0], [0,0,-1], [0,1,0] ]
    , testCase "-90° x-axis" $ rotmatrixForAxis xAxis (S¹ $ -π'₂)
                      @?≈ [ [1,0,0], [0,0,1], [0,-1,0] ]
    , testCase " 45° x-axis" $ rotmatrixForAxis xAxis (S¹ π'₄)
                      @?≈ [ [1,0,0], [0,sqrt 2/2,-sqrt 2/2], [0,sqrt 2/2,sqrt 2/2] ]
    , testCase "180° y-axis" $ rotmatrixForAxis yAxis (S¹ π)
                      @?≈ [ [-1,0,0], [0,1,0], [0,0,-1] ]
    , testCase " 90° y-axis" $ rotmatrixForAxis yAxis (S¹ π'₂)
                      @?≈ [ [0,0,1], [0,1,0], [-1,0,0] ]
    , testCase "-90° y-axis" $ rotmatrixForAxis yAxis (S¹ $ -π'₂)
                      @?≈ [ [0,0,-1], [0,1,0], [1,0,0] ]
    , testCase " 45° y-axis" $ rotmatrixForAxis yAxis (S¹ π'₄)
                      @?≈ [ [sqrt 2/2,0,sqrt 2/2], [0,1,0], [-sqrt 2/2,0,sqrt 2/2] ]
    , testCase "180° z-axis" $ rotmatrixForAxis zAxis (S¹ π)
                      @?≈ [ [-1,0,0], [0,-1,0], [0,0,1] ]
    , testCase " 90° z-axis" $ rotmatrixForAxis zAxis (S¹ π'₂)
                      @?≈ [ [0,-1,0], [1,0,0], [0,0,1] ]
    , testCase "-90° z-axis" $ rotmatrixForAxis zAxis (S¹ $ -π'₂)
                      @?≈ [ [0,1,0], [-1,0,0], [0,0,1] ]
    , testCase " 45° z-axis" $ rotmatrixForAxis zAxis (S¹ π'₄)
                      @?≈ [ [sqrt 2/2,-sqrt 2/2,0], [sqrt 2/2,sqrt 2/2,0], [0,0,1] ]
    ]
 , testGroup "Euler angles"
    [ testCase "180° x-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis xAxis (S¹ π))
                      @?≈ [-π'₄,  π , π³₄]
    , testCase " 90° x-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis xAxis (S¹ π'₂))
                      @?≈ [-π'₂, π'₂, π'₂]
    , testCase "-90° x-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis xAxis (S¹ $ -π'₂))
                      @?≈ [ π'₂, π'₂,-π'₂]
    , testCase " 45° x-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis xAxis (S¹ π'₄))
                      @?≈ [-π'₂, π'₄, π'₂]
    , testCase "180° y-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis yAxis (S¹ π))
                      @?≈ [ π'₄, π  , π'₄]
    , testCase " 90° y-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis yAxis (S¹ π'₂))
                      @?≈ [ 0  , π'₂, 0  ]
    , testCase "-90° y-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis yAxis (S¹ $ -π'₂))
                      @?≈ [ π  , π'₂, π  ]
    , testCase " 45° y-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis yAxis (S¹ π'₄))
                      @?≈ [ 0  , π'₄, 0  ]
    , testCase "180° z-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis zAxis (S¹ π))
                      @?≈ [ 0  ,  0 , π  ]
    , testCase " 90° z-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis zAxis (S¹ π'₂))
                      @?≈ [ 0  ,  0 , π'₂]
    , testCase "-90° z-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis zAxis (S¹ $ -π'₂))
                      @?≈ [ 0  ,  0 ,-π'₂]
    , testCase " 45° z-axis" $ eulerAnglesZYZForMatrix (rotmatrixForAxis zAxis (S¹ π'₄))
                      @?≈ [ 0  , 0  , π'₄]
    ]
 , testGroup "Concrete 180° rotations"
    [ testCase "x around x" $ rotateX (S¹ π) (S² π'₂ 0  ) @?≈ S² π'₂ 0
    , testCase "..and back" $ rotateX (S¹ $ -π)              (S² π'₂ 0     )@?≈ S² π'₂ 0
    , testCase "x around y" $ rotateY (S¹ π) (S² π'₂ 0  ) @?≈ S² π'₂ π
    , testCase "..and back" $ rotateY (S¹ $ -π)              (S² π'₂ π     )@?≈ S² π'₂ 0
    , testCase "x around z" $ rotateZ (S¹ π) (S² π'₂ 0  ) @?≈ S² π'₂ π
    , testCase "..and back" $ rotateZ (S¹ $ -π)              (S² π'₂ π     )@?≈ S² π'₂ 0
    , testCase "y around x" $ rotateX (S¹ π) (S² π'₂ π'₂) @?≈ S² π'₂ (-π'₂)
    , testCase "..and back" $ rotateX (S¹ $ -π)              (S² π'₂ (-π'₂))@?≈ S² π'₂ π'₂
    , testCase "y around y" $ rotateY (S¹ π) (S² π'₂ π'₂) @?≈ S² π'₂ π'₂
    , testCase "..and back" $ rotateY (S¹ $ -π)              (S² π'₂ π'₂   )@?≈ S² π'₂ π'₂
    , testCase "y around z" $ rotateZ (S¹ π) (S² π'₂ π'₂) @?≈ S² π'₂ (-π'₂)
    , testCase "..and back" $ rotateZ (S¹ $ -π)              (S² π'₂ (-π'₂))@?≈ S² π'₂ π'₂
    , testCase "z around x" $ rotateX (S¹ π) (S² 0   0  ) @?≈ S² π   0
    , testCase "..and back" $ rotateX (S¹ $ -π)              (S² π   0     )@?≈ S² 0   0
    , testCase "z around y" $ rotateY (S¹ π) (S² 0   0  ) @?≈ S² π   0
    , testCase "..and back" $ rotateY (S¹ $ -π)              (S² π   0     )@?≈ S² 0   0
    , testCase "z around z" $ rotateZ (S¹ π) (S² 0   0  ) @?≈ S² 0   0
    , testCase "..and back" $ rotateZ (S¹ $ -π)              (S² 0   0     )@?≈ S² 0   0
    ]
 , testGroup "Concrete 90° rotations"
    [ testCase "x around x" $ rotateX (S¹ π'₂) (S² π'₂ 0  ) @?≈ S² π'₂ 0
    , testCase "..and back" $ rotateX (S¹ $ -π'₂)              (S² π'₂ 0     )@?≈ S² π'₂ 0
    , testCase "x around y" $ rotateY (S¹ π'₂) (S² π'₂ 0  ) @?≈ S² π   0
    , testCase "..and back" $ rotateY (S¹ $ -π'₂)              (S² π   0     )@?≈ S² π'₂ 0
    , testCase "x around z" $ rotateZ (S¹ π'₂) (S² π'₂ 0  ) @?≈ S² π'₂ π'₂
    , testCase "..and back" $ rotateZ (S¹ $ -π'₂)              (S² π'₂ π'₂   )@?≈ S² π'₂ 0
    , testCase "y around x" $ rotateX (S¹ π'₂) (S² π'₂ π'₂) @?≈ S² 0   0
    , testCase "..and back" $ rotateX (S¹ $ -π'₂)              (S² 0   0     )@?≈ S² π'₂ π'₂
    , testCase "y around y" $ rotateY (S¹ π'₂) (S² π'₂ π'₂) @?≈ S² π'₂ π'₂
    , testCase "..and back" $ rotateY (S¹ $ -π'₂)              (S² π'₂ π'₂   )@?≈ S² π'₂ π'₂
    , testCase "y around z" $ rotateZ (S¹ π'₂) (S² π'₂ π'₂) @?≈ S² π'₂ π
    , testCase "..and back" $ rotateZ (S¹ $ -π'₂)              (S² π'₂ π     )@?≈ S² π'₂ π'₂
    , testCase "z around x" $ rotateX (S¹ π'₂) (S² 0   0  ) @?≈ S² π'₂ (-π'₂)
    , testCase "..and back" $ rotateX (S¹ $ -π'₂)              (S² π'₂ (-π'₂))@?≈ S² 0   0
    , testCase "z around y" $ rotateY (S¹ π'₂) (S² 0   0  ) @?≈ S² π'₂ 0
    , testCase "..and back" $ rotateY (S¹ $ -π'₂)              (S² π'₂ 0     )@?≈ S² 0   0
    , testCase "z around z" $ rotateZ (S¹ π'₂) (S² 0   0  ) @?≈ S² 0   π'₂
    , testCase "..and back" $ rotateZ (S¹ $ -π'₂)              (S² 0   π'₂   )@?≈ S² 0   0
    
    , testCase "around y, over x" $ rotateY (S¹ π'₂) (S² π'₄  0   ) @?≈ S² π³₄ 0
    , testCase "around z, over x" $ rotateZ (S¹ π'₂) (S² π'₂(-π'₄)) @?≈ S² π'₂ π'₄
    , testCase "around x, over y" $ rotateX (S¹ π'₂) (S² π³₄  π'₂ ) @?≈ S² π'₄ π'₂
    , testCase "around z, over y" $ rotateZ (S¹ π'₂) (S² π'₂  π'₄ ) @?≈ S² π'₂ π³₄
    , testCase "around x, over z" $ rotateX (S¹ π'₂) (S² π'₄  π'₂ ) @?≈ S² π'₄ (-π'₂)
    , testCase "around y, over z" $ rotateY (S¹ π'₂) (S² π'₄  π   ) @?≈ S² π'₄ 0
    ]
 , testGroup "Concrete 45° rotations"
    [ testCase "x around x" $ rotateX (S¹ π'₄) (S² π'₂ 0  ) @?≈ S² π'₂ 0
    , testCase "x around y" $ rotateY (S¹ π'₄) (S² π'₂ 0  ) @?≈ S² π³₄ 0
    , testCase "x around z" $ rotateZ (S¹ π'₄) (S² π'₂ 0  ) @?≈ S² π'₂ π'₄
    , testCase "y around x" $ rotateX (S¹ π'₄) (S² π'₂ π'₂) @?≈ S² π'₄ π'₂
    , testCase "y around y" $ rotateY (S¹ π'₄) (S² π'₂ π'₂) @?≈ S² π'₂ π'₂
    , testCase "y around z" $ rotateZ (S¹ π'₄) (S² π'₂ π'₂) @?≈ S² π'₂ π³₄
    , testCase "z around x" $ rotateX (S¹ π'₄) (S² 0   0  ) @?≈ S² π'₄ (-π'₂)
    , testCase "z around y" $ rotateY (S¹ π'₄) (S² 0   0  ) @?≈ S² π'₄ 0
    , testCase "z around z" $ rotateZ (S¹ π'₄) (S² 0   0  ) @?≈ S² 0   π'₂
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
  S¹ φ ≈ S¹ ϕ
   | φ > pi/2, ϕ < -pi/2  = S¹ (φ - 2*pi) ≈ S¹ ϕ
   | ϕ > pi/2, φ < -pi/2  = S¹ φ ≈ S¹ (ϕ - 2*pi)
   | otherwise            = φ ≈ ϕ
instance AEq S² where
  S² θ φ ≈ S² ϑ ϕ
   | φ > pi/2, ϕ < -pi/2  = S² θ (φ - 2*pi) ≈ S² ϑ ϕ
   | ϕ > pi/2, φ < -pi/2  = S² θ φ ≈ S² ϑ (ϕ - 2*pi)
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

xAxis, yAxis, zAxis :: ℝP²
xAxis = ℝP² 1 0
yAxis = ℝP² 1 (pi/2)
zAxis = ℝP² 0 0

