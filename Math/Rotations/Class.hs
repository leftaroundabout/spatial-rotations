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

         -- Rotation matrix for z₀-y-z₁ rotation, with cy := cos θy etc.:
         --
         -- ⎛ r₀₀ r₀₁ r₀₂ ⎞   ⎛ cz₁ -sz₁ 0 ⎞   ⎛ cy  0 -sy ⎞   ⎛ cz₀ -sz₀ 0 ⎞
         -- ⎜ r₁₀ r₁₁ r₁₂ ⎟ = ⎜ sz₁  cz₁ 0 ⎟ · ⎜ 0   1  0  ⎟ · ⎜ sz₀  cz₀ 0 ⎟
         -- ⎝ r₂₀ r₂₁ r₂₂ ⎠   ⎝ 0    0   1 ⎠   ⎝ sy  0  cy ⎠   ⎝ 0    0   1 ⎠
         --
         --      ⎛ cz₁ -sz₁ 0 ⎞   ⎛ cy·cz₀ -cy·sz₀ -sy ⎞
         --    = ⎜ sz₁  cz₁ 0 ⎟ · ⎜  sz₀     cz₀    0  ⎟
         --      ⎝  0    0  1 ⎠   ⎝ sy·cz₀    0     cy ⎠
         --
         --      ⎛ cy·cz₀·cz₁−sz₀·sz₁  -cz₁·sz₀−cy·cz₀·sz₁   sy·cz₀ ⎞
         --    = ⎜ cy·cz₁·sz₀+cz₀·sz₁   cz₀·cz₁−cy·sz₀·sz₁   sy·sz₀ ⎟
         --      ⎝      -sy·cz₁               sy·sz₁           cy   ⎠
         --
         -- Here, one can immediately read off
              cy = r₂₂
         -- ...but the naïve choice
         --    θy = acos cy = acos r₂₂
         -- is unstable. Better:
         --  sqrt(r₂₀²+r₂₁²) = |sy|·(cz₁²+sz₁²) = sy
              sy = sqrt $ r₂₀^2+r₂₁^2
              θy = atan2 sy cy
         -- We can always choose
         --   θz₀ = atan2 sz₀ cz₀
         --       = atan2 (sy·sz₀) (sy·cz₀)  ∀sy‡0
              θz₀ = atan2 r₁₂ r₀₂  ; sz₀ = sin θz₀; cz₀ = cos θz₀
         -- ...noting however that this becomes underconstrained for small |sy|, so
         -- the analogous θz₁ = atan2 r₂₁ (-r₂₀) should /not/ be used. Instead, put
         -- in the x unit vector turned back by θy and θz₀:
         -- ⎛ r₀₀ r₀₁ r₀₂ ⎞   ⎛ cy·cz₀ -cy·sz₀ -sy ⎞⁻¹  ⎛1⎞   ⎛ cz₁ -sz₁ 0 ⎞⎛1⎞   ⎛cz₁⎞
         -- ⎜ r₁₀ r₁₁ r₁₂ ⎟ · ⎜  sz₀     cz₀    0  ⎟  $ ⎜0⎟ = ⎜ sz₁  cz₁ 0 ⎟⎜0⎟ = ⎜sz₁⎟
         -- ⎝ r₂₀ r₂₁ r₂₂ ⎠   ⎝ sy·cz₀    0     cy ⎠    ⎝0⎠   ⎝  0    0  1 ⎠⎝0⎠   ⎝ 0 ⎠
         --
         -- Here we have, using orthogonality,
         -- ⎛ cy·cz₀ -cy·sz₀ -sy ⎞⁻¹⎛1⎞   ⎛  cy·cz₀  sz₀ sy·cz₀ ⎞⎛1⎞   ⎛ cy·cz₀ ⎞
         -- ⎜  sz₀     cz₀    0  ⎟  ⎜0⎟ = ⎜ -cy·sz₀  cz₀   0    ⎟⎜0⎟ = ⎜-cy·sz₀ ⎟
         -- ⎝ sy·cz₀    0     cy ⎠  ⎝0⎠   ⎝   -sy     0    cy   ⎠⎝0⎠   ⎝  -sy   ⎠
         -- 
              θz₁ = atan2 (r₁₀*cy*cz₀ - r₁₁*cy*sz₀ - r₁₂*sy)
                          (r₀₀*cy*cz₀ - r₀₁*cy*sz₀ - r₀₂*sy)
       
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
