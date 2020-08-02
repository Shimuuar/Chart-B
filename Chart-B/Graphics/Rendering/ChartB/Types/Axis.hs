{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Data types and type classes for working with plot axes.
module Graphics.Rendering.ChartB.Types.Axis
  ( -- * Axis
    Axis(..)
  , AxisParam(..)
  , axisLimits
  , axisLogScale
  , AxisTransform(..)
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

import Control.Lens
import Data.Coerce
import Data.Default.Class
import Data.List (minimumBy)
import Data.Monoid
import Data.Ord
import Graphics.Rendering.ChartB.Types.Property


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
  -- | Convert axis value into range estimator
  axisEsimator :: AxisParam a -> AxisValue a -> AxisRangeEst a
  -- | Convert range estimate from data to the data relevant for making plot
  makeAxisTransform :: AxisParam a -> AxisRangeEst a -> AxisTransform a

-- | Estimate range for the axis for given data
estimateRange
  :: forall x y. (Axis x, Axis y)
  => FoldOverAxes x y  -- ^
  -> AxisParam x       -- ^
  -> AxisParam y       -- ^
  -> (AxisRangeEst x, AxisRangeEst y)
estimateRange points axisX axisY = (rX,rY)
  where
    Pair rX rY
      = (foldOverAxes points)
        (\(Pair mX mY) x y -> Pair (mX <> axisEsimator axisX x) (mY <> axisEsimator axisY y))
        (\(Pair mX mY) x   -> Pair (mX <> axisEsimator axisX x)  mY)
        (\(Pair mX mY)   y -> Pair  mX                          (mY <> axisEsimator axisY y))
        mempty

-- | Named tick on an axis.
data Tick a = Tick
  { tickLabel :: String
  , tickValue :: AxisValue a
  }
deriving instance (Show (AxisValue a)) => Show (Tick a)

-- | Describes how to transform data for plotting
data AxisTransform a = AxisTransform
  { axisTicks     :: [Tick a]
    -- ^ Range of data
  , axisPlotRange :: (Double,Double)
    -- ^ Range of in plot coordinates
  , axisPointMap  :: AxisValue a -> Double
    -- ^ Function to transform data into plot coordinates
  }

-- | Parameter for axis.
data AxisParam a = AxisParam
  { _axisLimits   :: !(Maybe (AxisValue a), Maybe (AxisValue a))
    -- ^ User specified limits.
  , _axisLogScale :: Bool
    -- ^ Whether to use log scale.
  }


instance Default (AxisParam a) where
  def = AxisParam
    { _axisLimits   = (Nothing,Nothing)
    , _axisLogScale = False
    }

instance Semigroup (AxisParam a) where
  a <> b = AxisParam
    { _axisLimits   = onFirst (_axisLimits a) (_axisLimits b)
    , _axisLogScale = _axisLogScale a || _axisLogScale b
    }
    where
      onFirst :: forall b. (Maybe b, Maybe b) -> (Maybe b, Maybe b) -> (Maybe b, Maybe b)
      onFirst x y = coerce (coerce x <> coerce y :: (First b, First b))

instance Monoid (AxisParam a) where
  mempty = def


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
  axisEsimator p x
    | _axisLogScale p && x < 0 = mempty
    | otherwise                = case _axisLimits p of
        (Nothing, Nothing)                    -> est
        (Just a,  Nothing) | x >= a           -> est
        (Nothing, Just b ) | x <= b           -> est
        (Just a,  Just b ) | x >= a && x <= b -> est
        _                                     -> mempty
    where
      est = MinMaxLimits x x
  --
  makeAxisTransform AxisParam{..} rangeLin = AxisTransform
    { axisTicks = if
        | _axisLogScale -> []
        | otherwise     -> numericTicks 5 spanLin
    , axisPlotRange = if
        | _axisLogScale -> fromRange rangeLog limitsLog
        | otherwise     -> spanLin
    , axisPointMap  = if
        | _axisLogScale -> logBase 10
        | otherwise     -> id
    }
    where
      spanLin = fromRange rangeLin limitsLin
      --
      limitsLin = normalizeRange _axisLimits
      limitsLog = limitsLin & both %~ (\m -> [ logBase 10 x | x <- m, x > 0 ])
      -- NOTE: we filter negative values when estimating range
      rangeLog = case rangeLin of
        UnknownLim       -> UnknownLim
        MinMaxLimits x y -> MinMaxLimits (logBase 10 x) (logBase 10 y)

normalizeRange :: Ord a => (Maybe a, Maybe a) -> (Maybe a, Maybe a)
normalizeRange (Just a, Just b) | a > b = (Just b, Just a)
normalizeRange r                        = r

fromRange :: AxisRangeEst Numeric -> (Maybe Double, Maybe Double) -> (Double, Double)
fromRange _          (Just a , Just b)  = (a   , b  )
fromRange UnknownLim (Nothing, Nothing) = (0   , 1  )
fromRange UnknownLim (Just a,  Nothing) = (a   , a+1)
fromRange UnknownLim (Nothing, Just b)  = (b-1 , b  )
fromRange (MinMaxLimits a b) (Nothing, Nothing)
  | a == b    = (a - 0.5, a + 0.5)
  | otherwise = (a - 0.05*d, b + 0.05*d)
  where d = b - a
fromRange (MinMaxLimits _ b) (Just a, Nothing)
  | b' > a    = (a, b + 0.05*d)
  | otherwise = (a, a+1)
  where b' = max a b
        d  = b' - a
fromRange (MinMaxLimits a _) (Nothing, Just b)
  | a < b     = (a' - 0.05*d, b)
  | otherwise = (b-1, b)
  where a' = min a b
        d  = b - a'

numericTicks :: Int -> (AxisValue Numeric, AxisValue Numeric) -> [Tick Numeric]
numericTicks nTicks (a,b) =
  [ Tick (show x) x | x <- realToFrac <$> steps (fromIntegral nTicks) (a, b) ]

steps :: RealFloat a => a -> (a,a) -> [Rational]
steps nSteps rs@(minV,maxV) = map ((s*) . fromIntegral) [min' .. max']
  where
    s    = chooseStep nSteps rs
    min' :: Integer
    min' = ceiling $ realToFrac minV / s
    max' = floor   $ realToFrac maxV / s

chooseStep :: RealFloat a => a -> (a,a) -> Rational
chooseStep nsteps (x1,x2) = minimumBy (comparing proximity) stepVals
  where
    delta = x2 - x1
    mult  | delta == 0 = 1  -- Otherwise the case below will use all of memory
          | otherwise  = 10 ^^ ((floor $ log10 $ delta / nsteps)::Int)
    stepVals    = map (mult*) [0.1,0.2,0.25,0.5,1.0,2.0,2.5,5.0,10,20,25,50]
    proximity x = abs $ delta / realToFrac x - nsteps

log10 :: (Floating a) => a -> a
log10 = logBase 10


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


$(makeLenses ''AxisParam)

instance ( lim ~ AxisValue a, lim' ~ AxisValue a
         ) => IsLabel "lim" (Property (Maybe lim, Maybe lim') (AxisParam a)) where
  fromLabel = Property axisLimits

instance IsLabel "log" (Property Bool (AxisParam a)) where
  fromLabel = Property axisLogScale
