{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
-- |
-- Data types and type classes for working with plot axes.
module Graphics.Rendering.ChartB.Types.Axis
  ( -- * Axis
    Axis(..)
  , Tick(..)
    -- ** Concrete axes
  , Numeric
  , AxisRangeEst(UnknownLim,MinMaxLimits)
    -- * Range estimation
  , FoldOverAxes(..)
  , filterAxisX
  , filterAxisY
  , estimateRange
  ) where

import Data.Proxy


----------------------------------------------------------------
-- Generic API for axes
----------------------------------------------------------------

-- | Fold which is used to compute maximum and minimum value for
--   axis autorange
newtype FoldOverAxes x y = FoldOverAxes
  { foldOverAxes :: forall a. (a -> AxisValue x -> AxisValue y -> a)
                           -> (a -> AxisValue x -> a)
                           -> (a -> AxisValue y -> a)
                           -> (a -> a)
  }

instance Semigroup (FoldOverAxes x y) where
  FoldOverAxes f <> FoldOverAxes g = FoldOverAxes $ \stepXY stepX stepY ->
    g stepXY stepX stepY . f stepXY stepX stepY

instance Monoid (FoldOverAxes x y) where
  mempty = FoldOverAxes $ \_ _ _ -> id

-- | Only fold over values with X coordinate satisfying predicate
filterAxisX :: (AxisValue x -> Bool) -> FoldOverAxes x y -> FoldOverAxes x y
filterAxisX predX (FoldOverAxes fun) = FoldOverAxes $ \stepXY stepX
  -> fun (\a x y -> if predX x then stepXY a x y else a)
         (\a x   -> if predX x then stepX  a x   else a)

-- | Only fold over values with Y coordinate satisfying predicate
filterAxisY :: (AxisValue y -> Bool) -> FoldOverAxes x y -> FoldOverAxes x y
filterAxisY predY (FoldOverAxes fun) = FoldOverAxes $ \stepXY stepX stepY
  -> fun (\a x y -> if predY y then stepXY a x y else a)
         stepX
         (\a y   -> if predY y then stepY  a y   else a)



-- | Type class for axis which is classified according to what kind of
--   data is stored there.
class Monoid (AxisRangeEst a) => Axis a where
  -- | Value whis used for plotting. For example it's 'Double' for
  --   'Numeric' axes. However it's possible to use value that could
  --   be converted to that type.
  type AxisValue    a
  -- | Constraint on ais range provided by user.
  data AxisRangeEst a
  -- | True if value is within axis range
  axisValueInRange :: Proxy a -> (Maybe (AxisValue a), Maybe (AxisValue a)) -> AxisValue a -> Bool
  -- | Convert axis value into range estimator
  axisEsimator :: AxisValue a -> AxisRangeEst a

-- | Estimate range for the axis for given data
estimateRange
  :: forall x y. (Axis x, Axis y)
  => FoldOverAxes x y           -- ^
  -> (Maybe (AxisValue x), Maybe (AxisValue x)) -- ^
  -> (Maybe (AxisValue y), Maybe (AxisValue y)) -- ^
  -> (AxisRangeEst x, AxisRangeEst y)
estimateRange points rngX rngY = (rX,rY)
  where
    Pair rX rY
      = ( foldOverAxes
        $ filterAxisX (axisValueInRange (Proxy @x) rngX)
        $ filterAxisY (axisValueInRange (Proxy @y) rngY)
        $ points
        )
        (\(Pair mX mY) x y -> Pair (mX <> axisEsimator x) (mY <> axisEsimator y))
        (\(Pair mX mY) x   -> Pair (mX <> axisEsimator x)  mY)
        (\(Pair mX mY)   y -> Pair  mX                    (mY <> axisEsimator y))
        mempty


-- | Named tick on an axis.
data Tick a = Tick
  { tickLabel :: String
  , tickValue :: AxisValue a
  }



----------------------------------------------------------------
-- Numeric axis
----------------------------------------------------------------

-- | Axis for numeric data.
data Numeric

instance Axis Numeric where
  type AxisValue    Numeric = Double
  data AxisRangeEst Numeric
    = UnknownLim
    | MinMaxLimits !Double !Double
    deriving (Show)
  --
  axisValueInRange _ (Nothing, Nothing) _ = True
  axisValueInRange _ (Just a,  Nothing) x = x >= a
  axisValueInRange _ (Nothing, Just b ) x = x <= b
  axisValueInRange _ (Just a,  Just b ) x = x >= a && x <= b
  --
  axisEsimator x = MinMaxLimits x x

instance Semigroup (AxisRangeEst Numeric) where
  UnknownLim <> x = x
  x <> UnknownLim = x
  MinMaxLimits a1 b1 <> MinMaxLimits a2 b2 = MinMaxLimits (min a1 a2) (max b1 b2)

instance Monoid (AxisRangeEst Numeric) where
  mempty = UnknownLim



----------------------------------------------------------------
--
----------------------------------------------------------------

data Pair a b = Pair !a !b
              deriving (Show,Eq,Ord)

instance (Semigroup a, Semigroup b) => Semigroup (Pair a b) where
  Pair x y <> Pair x' y' = Pair (x <> x') (y <> y')
  {-# INLINABLE (<>) #-}

instance (Monoid a, Monoid b) => Monoid (Pair a b) where
  mempty = Pair mempty mempty
  Pair x y `mappend` Pair x' y' = Pair (x `mappend` x') (y `mappend` y')
  {-# INLINABLE mempty  #-}
  {-# INLINABLE mappend #-}
