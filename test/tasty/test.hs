-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
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
 [ testGroup "Concrete 90° rotations"
    [ testCase "x around x" $ rotateX (S¹ π'₂) (S² π'₂ 0  ) @?≈ S² π'₂ 0
    , testCase "x around y" $ rotateY (S¹ π'₂) (S² π'₂ 0  ) @?≈ S² pi  0
    , testCase "x around z" $ rotateZ (S¹ π'₂) (S² π'₂ 0  ) @?≈ S² π'₂ π'₂
    , testCase "y around x" $ rotateX (S¹ π'₂) (S² π'₂ π'₂) @?≈ S² 0   0
    , testCase "y around y" $ rotateY (S¹ π'₂) (S² π'₂ π'₂) @?≈ S² π'₂ π'₂
    , testCase "y around z" $ rotateZ (S¹ π'₂) (S² π'₂ π'₂) @?≈ S² π'₂ π
    , testCase "z around x" $ rotateX (S¹ π'₂) (S² 0   0  ) @?≈ S² π'₂ (-π'₂)
    , testCase "z around y" $ rotateY (S¹ π'₂) (S² 0   0  ) @?≈ S² π'₂ 0
    , testCase "z around z" $ rotateZ (S¹ π'₂) (S² 0   0  ) @?≈ S² 0   π'₂
    ]
 ]
 where π = pi
       π'₂ = pi/2



infix 4 ≈
class AEq e where
  (≈) :: e -> e -> Bool

instance AEq S¹ where
  S¹ φ ≈ S¹ ϕ
   | φ > pi/2, ϕ < -pi/2  = S¹ (φ - 2*pi) ≈ S¹ ϕ
   | ϕ > pi/2, φ < -pi/2  = S¹ φ ≈ S¹ (ϕ - 2*pi)
   | otherwise            = abs (φ - ϕ) < 1e-9
instance AEq S² where
  S² θ φ ≈ S² ϑ ϕ
   | φ > pi/2, ϕ < -pi/2  = S² θ (φ - 2*pi) ≈ S² ϑ ϕ
   | ϕ > pi/2, φ < -pi/2  = S² θ φ ≈ S² ϑ (ϕ - 2*pi)
   | otherwise            = abs (θ - ϑ) < 1e-9 && abs (φ - ϕ) * sin θ < 1e-9

                  
infix 1 @?≈       
(@?≈) :: (AEq e, Show e) => e -> e -> Assertion
a@?≈b
 | a≈b        = return ()
 | otherwise  = assertFailure $ "Expected "++show b++", but got "++show a


