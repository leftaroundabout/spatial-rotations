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

module Math.Rotations.Class where

import Math.Manifold.Core.Types
import Data.VectorSpace

class Rotatable m where
  type AxisSpace m :: *
  rotateAbout :: AxisSpace m -> S¹ -> m -> m

