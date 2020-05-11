{-# LANGUAGE TypeFamilies #-}
-- |
module Graphics.Rendering.ChartB.Impl.Axis where

import Data.Proxy

-- | Type class for axis which is classified according to what kind of
--   data is stored there.
class Axis a where
  type AxisValue a
  -- | True if value
  axisValueInRange
    :: Proxy a
    -- ^ Dummy for type inference
    -> (Maybe (AxisValue a), Maybe (AxisValue a))
    -- ^ Range for axis
    -> AxisValue a
    -- ^ Value to check
    -> Bool


----------------------------------------------------------------
-- Numeric axis
----------------------------------------------------------------

-- | Axis for numeric data.
data Numeric

instance Axis Numeric where
  type AxisValue Numeric = Double
  axisValueInRange _ (Nothing, Nothing) _ = True
  axisValueInRange _ (Just a,  Nothing) x = x >= a
  axisValueInRange _ (Nothing, Just b ) x = x <= b
  axisValueInRange _ (Just a,  Just b ) x = x >= a && x <= b



data NumLimits
  = UnknownLim
  | MinMaxLimits !Double !Double
  deriving (Show)

numLim :: Double -> NumLimits
numLim x = MinMaxLimits x x

instance Semigroup NumLimits where
  UnknownLim <> x = x
  x <> UnknownLim = x
  MinMaxLimits a1 b1 <> MinMaxLimits a2 b2 = MinMaxLimits (min a1 a2) (max b1 b2)

instance Monoid NumLimits where
  mempty = UnknownLim
