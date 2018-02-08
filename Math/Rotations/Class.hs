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

module Math.Rotations.Class ( Rotatable (..)
                            , rotateX, rotateY, rotateZ
                            ) where

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

rotateViaEulerAnglesYZ
  :: (S¹ -> m -> m)        -- ^ Y-axis rotation method
  -> (S¹ -> m -> m)        -- ^ Z-axis rotation method
  -> (ℝP² -> S¹ -> m -> m) -- ^ Suitable definition for 'rotateAbout'
rotateViaEulerAnglesYZ yRot zRot (ℝP² rax φax) = rotAroundAxis
 where rotAroundAxis (S¹ θ) = zRot (S¹ θz₀) . yRot (S¹ θy) . zRot (S¹ θz₁)
        where cosθ = cos θ
              sinθ = sin θ
              one_cosθ = 1 - cos θ
              
         -- https://en.wikipedia.org/w/index.php?title=Rotation_formalisms_in_three_dimensions&oldid=823164970#Rotation_matrix_%E2%86%94_Euler_axis/angle
              r₀₀ = one_cosθ*e₀^2  + cosθ
              r₀₁ = one_cosθ*e₀*e₁ - e₂*sinθ
              r₀₂ = one_cosθ*e₀*e₂ + e₁*sinθ
              r₁₀ = one_cosθ*e₁*e₀ + e₂*sinθ
              r₁₁ = one_cosθ*e₁^2  + cosθ
              r₁₂ = one_cosθ*e₁*e₂ - e₀*sinθ
              r₂₀ = one_cosθ*e₂*e₀ - e₁*sinθ
              r₂₁ = one_cosθ*e₂*e₁ + e₀*sinθ
              r₂₂ = one_cosθ*e₂^2  + cosθ

         -- https://www.geometrictools.com/Documentation/EulerAngles.pdf
              ( θy
               ,θz₀
               ,θz₁ )
               | not (r₂₂ < 1)   = (0, atan2 r₁₀ r₁₁, 0)
               | not (r₂₂ > -1)  = (pi, -atan2 r₁₀ r₁₁, 0)
               | otherwise   = ( acos r₂₂
                               , atan2 r₁₂ r₀₂
                               , atan2 r₂₁ (-r₂₀) )
       
       θax = pi/2 * rax
       e₀ = cos φax * sin θax
       e₁ = sin φax * sin θax
       e₂ = cos θax


instance Rotatable S² where
  type AxisSpace S² = ℝP²
  rotateAbout = rotateViaEulerAnglesYZ
       (\(S¹ β) (S² θ φ)
           -> let x₀ = cos φ * sin θ
                  y  = sin φ * sin θ
                  z₀ = cos θ
                  x₁ =  x₀ * cos β + z₀ * sin β
                  z₁ = -x₀ * sin β + z₀ * cos β
                  rxy = sqrt $ x₁^2 + y^2
              in S² (atan2 rxy z₁) (atan2 y x₁) )
       (\γ (S² θ φ) -> case rotateAbout ℝPZero γ (S¹ φ) of S¹ φ' -> S² θ φ')

rotateX, rotateY, rotateZ :: (Rotatable m, AxisSpace m ~ ℝP²)
           => S¹ -> m -> m
rotateX = rotateAbout $ ℝP² 1 0
rotateY = rotateAbout $ ℝP² 1 (pi/2)
rotateZ = rotateAbout $ ℝP² 0 0

tau :: ℝ
tau = 2*pi
