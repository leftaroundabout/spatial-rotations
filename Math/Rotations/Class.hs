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
{-# LANGUAGE FlexibleInstances        #-}

module Math.Rotations.Class ( Rotatable (..)
                            , xAxis, yAxis, zAxis
                            , (°)
                            -- * Utility
                            , rotateViaEulerAnglesYZ
                            , rotateℝ³AboutCenteredAxis
                            -- * Internals
                            , rotmatrixForAxis
                            , eulerAnglesZYZForMatrix
                            , rotmatrixForEulerAnglesZYZ
                            ) where

import Math.Manifold.Core.Types
import Math.Manifold.Core.PseudoAffine
import Data.VectorSpace
import Linear.V3 (V3(V3))

class Rotatable m where
  type AxisSpace m :: *
  rotateAbout :: AxisSpace m -> S¹ -> m -> m

instance (Rotatable m) => Rotatable (m -> Double) where
  type AxisSpace (m -> Double) = AxisSpace m
  rotateAbout ax (S¹Polar δφ) f = f . rotateAbout ax (S¹Polar $ -δφ)

instance Rotatable S¹ where
  type AxisSpace S¹ = ℝP⁰
  rotateAbout ℝPZero (S¹Polar δφ) (S¹Polar φ)
    | φ' > pi    = S¹Polar $ φ'-tau
    | φ' < -pi   = S¹Polar $ φ'+tau
    | otherwise  = S¹Polar φ'
   where φ' = φ + δφ

rotateViaEulerAnglesYZ
  :: (S¹ -> m -> m)        -- ^ Y-axis rotation method
  -> (S¹ -> m -> m)        -- ^ Z-axis rotation method
  -> (ℝP² -> S¹ -> m -> m) -- ^ Suitable definition for 'rotateAbout'
rotateViaEulerAnglesYZ yRot zRot ax = rotAroundAxis . rotmatrixForAxis ax
 where rotAroundAxis mat = case eulerAnglesZYZForMatrix mat of
          [θz₀, θy, θz₁] -> zRot (S¹Polar θz₁) . yRot (S¹Polar θy) . zRot (S¹Polar θz₀)

rotmatrixForAxis :: ℝP² -> S¹ -> [[ℝ]]
rotmatrixForAxis (HemisphereℝP²Polar θax φax) = rotAroundAxis
 where rotAroundAxis (S¹Polar θ) = [[r₀₀,r₀₁,r₀₂]
                                   ,[r₁₀,r₁₁,r₁₂]
                                   ,[r₂₀,r₂₁,r₂₂]]
         -- https://en.wikipedia.org/w/index.php?title=Rotation_formalisms_in_three_dimensions&oldid=823164970#Rotation_matrix_%E2%86%94_Euler_axis/angle
        where r₀₀ = one_cosθ*e₀^2  + cosθ
              r₀₁ = one_cosθ*e₀*e₁ - e₂*sinθ
              r₀₂ = one_cosθ*e₀*e₂ + e₁*sinθ
              r₁₀ = one_cosθ*e₁*e₀ + e₂*sinθ
              r₁₁ = one_cosθ*e₁^2  + cosθ
              r₁₂ = one_cosθ*e₁*e₂ - e₀*sinθ
              r₂₀ = one_cosθ*e₂*e₀ - e₁*sinθ
              r₂₁ = one_cosθ*e₂*e₁ + e₀*sinθ
              r₂₂ = one_cosθ*e₂^2  + cosθ
              cosθ = cos θ
              sinθ = sin θ
              one_cosθ = 1 - cos θ
       
       e₀ = cos φax * sin θax
       e₁ = sin φax * sin θax
       e₂ = cos θax

rotmatrixForEulerAnglesZYZ :: [ℝ] -> [[ℝ]]
rotmatrixForEulerAnglesZYZ angles
              = [[ cy*cz₀*cz₁-sz₀*sz₁, -cy*sz₀*cz₁-cz₀*sz₁,  sy*cz₁ ]
                ,[ cy*cz₀*sz₁+cz₁*sz₀,  cz₀*cz₁-cy*sz₀*sz₁,  sy*sz₁ ]
                ,[      -sy*cz₀      ,        sy*sz₀      ,    cy   ]]
 where [cz₀,cy,cz₁] = cos<$>angles
       [sz₀,sy,sz₁] = sin<$>angles

eulerAnglesZYZForMatrix :: [[ℝ]] -> [ℝ]             
eulerAnglesZYZForMatrix [[r₀₀,r₀₁,r₀₂]
                        ,[r₁₀,r₁₁,r₁₂]
                        ,[r₂₀,r₂₁,r₂₂]]
           = [θz₀,θy,θz₁]
 where
         -- Rotation matrix for z₀-y-z₁ rotation, with cy := cos θy etc.:
         --
         -- ⎛ r₀₀ r₀₁ r₀₂ ⎞   ⎛ cz₁ -sz₁ 0 ⎞   ⎛ cy  0  sy ⎞   ⎛ cz₀ -sz₀ 0 ⎞
         -- ⎜ r₁₀ r₁₁ r₁₂ ⎟ = ⎜ sz₁  cz₁ 0 ⎟ · ⎜ 0   1  0  ⎟ · ⎜ sz₀  cz₀ 0 ⎟
         -- ⎝ r₂₀ r₂₁ r₂₂ ⎠   ⎝ 0    0   1 ⎠   ⎝-sy  0  cy ⎠   ⎝ 0    0   1 ⎠
         --
         --      ⎛ cz₁ -sz₁ 0 ⎞   ⎛ cy·cz₀ -cy·sz₀  sy ⎞
         --    = ⎜ sz₁  cz₁ 0 ⎟ · ⎜  sz₀     cz₀    0  ⎟
         --      ⎝  0    0  1 ⎠   ⎝-sy·cz₀  sy·sz₀  cy ⎠
         --
         --      ⎛ cy·cz₀·cz₁−sz₀·sz₁ -cy·sz₀·cz₁−cz₀·sz₁   sy·cz₁ ⎞
         --    = ⎜ cy·cz₀·sz₁+cz₁·sz₀  cz₀·cz₁−cy·sz₀·sz₁   sy·sz₁ ⎟
         --      ⎝      -sy·cz₀              sy·sz₀           cy   ⎠
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
              θz₀ = atan2 r₂₁ (-r₂₀)  ; sz₀ = sin θz₀; cz₀ = cos θz₀
         -- ...noting however that this becomes underconstrained for small |sy|, so
         -- the analogous θz₁ = atan2 r₂₁ (-r₂₀) should /not/ be used. Instead, put
         -- in the y unit vector turned back by θz₀ (θy has no effect):
         -- ⎛ r₀₀ r₀₁ r₀₂ ⎞   ⎛ cy·cz₀ -cy·sz₀  sy ⎞⁻¹  ⎛0⎞   ⎛ cz₁ -sz₁ 0 ⎞⎛0⎞   ⎛-sz₁⎞
         -- ⎜ r₁₀ r₁₁ r₁₂ ⎟ · ⎜  sz₀     cz₀    0  ⎟  $ ⎜1⎟ = ⎜ sz₁  cz₁ 0 ⎟⎜1⎟ = ⎜ cz₁⎟
         -- ⎝ r₂₀ r₂₁ r₂₂ ⎠   ⎝-sy·cz₀  sy·sz₀  cy ⎠    ⎝0⎠   ⎝  0    0  1 ⎠⎝0⎠   ⎝  0 ⎠
         --
         -- Here we have, using orthogonality,
         -- ⎛ cy·cz₀ -cy·sz₀  sy ⎞⁻¹⎛0⎞   ⎛  cy·cz₀  sz₀ -sy·cz₀ ⎞⎛0⎞   ⎛sz₀⎞
         -- ⎜  sz₀     cz₀    0  ⎟  ⎜1⎟ = ⎜ -cy·sz₀  cz₀  sy·sz₀ ⎟⎜1⎟ = ⎜cz₀⎟
         -- ⎝-sy·cz₀  sy·sz₀  cy ⎠  ⎝0⎠   ⎝    sy     0     cy   ⎠⎝0⎠   ⎝ 0 ⎠
              θz₁ = atan2 (-r₀₀*sz₀ - r₀₁*cz₀)
                          ( r₁₀*sz₀ + r₁₁*cz₀)

instance Rotatable S² where
  type AxisSpace S² = ℝP²
  rotateAbout = rotateViaEulerAnglesYZ
       (\(S¹Polar β) (S²Polar θ φ)
           -> let x₀ = cos φ * sin θ
                  y  = sin φ * sin θ
                  z₀ = cos θ
                  x₁ =  x₀ * cos β + z₀ * sin β
                  z₁ = -x₀ * sin β + z₀ * cos β
                  rxy = sqrt $ x₁^2 + y^2
              in S²Polar (atan2 rxy z₁) (atan2 y x₁) )
       (\γ (S²Polar θ φ) -> case rotateAbout ℝPZero γ (S¹Polar φ) of
                              S¹Polar φ' -> S²Polar θ φ')

xAxis, yAxis, zAxis :: ℝP²
xAxis = HemisphereℝP²Polar (pi/2) 0
yAxis = HemisphereℝP²Polar (pi/2) (pi/2)
zAxis = HemisphereℝP²Polar 0      0

infix 5 °

-- | Rotate by an angle specified in degrees.
(°) :: Rotatable m => ℝ -> AxisSpace m -> m -> m
angle° axis = rotateAbout axis . S¹Polar $ angle * pi/180


rotateℝ³AboutCenteredAxis :: ℝP² -> S¹ -> V3 ℝ -> V3 ℝ
rotateℝ³AboutCenteredAxis axis angle = case rotmatrixForAxis axis angle of
     [ [r₀₀,r₀₁,r₀₂]
      ,[r₁₀,r₁₁,r₁₂]
      ,[r₂₀,r₂₁,r₂₂] ] -> \(V3 x y z) -> V3 (r₀₀*x + r₀₁*y + r₀₂*z)
                                            (r₁₀*x + r₁₁*y + r₁₂*z)
                                            (r₂₀*x + r₂₁*y + r₂₂*z)
