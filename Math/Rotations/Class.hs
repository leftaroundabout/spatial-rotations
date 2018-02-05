-- |
-- Module      : Math.Rotations.Class
-- Copyright   : (c) Justus Sagemüller 2018
-- License     : GPL v3
-- 
-- Maintainer  : (@) jsagemue $ uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

{-# LANGUAGE TypeFamilies             #-}

module Math.Rotations.Class ( Rotatable (..) ) where

import Math.Manifold.Core.Types
import Data.VectorSpace

class Rotatable m where
  type AxisSpace m :: *
  rotateAbout :: AxisSpace m -> S¹ -> m -> m


instance Rotatable S¹ where
  type AxisSpace S¹ = ℝP⁰
  rotateAbout ℝPZero (S¹ δφ) (S¹ φ)
    | φ' > pi    = S¹ $ φ-tau
    | φ' < -pi   = S¹ $ φ+tau
    | otherwise  = S¹ φ'
   where φ' = φ + δφ

tau :: ℝ
tau = 2*pi
