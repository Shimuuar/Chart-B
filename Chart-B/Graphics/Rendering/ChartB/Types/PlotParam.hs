{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
module Graphics.Rendering.ChartB.Types.PlotParam where

import Control.Category
import Control.Lens
import Data.Colour
import Data.Default.Class
import Data.Distributive
import Data.Monoid
import GHC.OverloadedLabels (IsLabel(..))
import Prelude hiding ((.),id)

import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.ChartB.Types.Property


----------------------------------------------------------------
-- Plot parameters
----------------------------------------------------------------

-- | Parameters for a single plot entity: line, barplot etc. Only part
--   of parameters relevant to plot will be used rest will be ignores.
data PlotParam = PlotParam
  { _plotMainColor :: !(AlphaColour Double) -- ^ Primary color of everything
  , _plotMarker    :: !MarkerParam          -- ^ Style of markers
  , _plotLines     :: !LineParam            -- ^ Style of lines
  , _plotFill      :: !FillParam            -- ^ Fill style
  }

instance Default PlotParam where
  def = PlotParam
    { _plotMainColor = opaque black
    , _plotMarker    = def
    , _plotLines     = def
    , _plotFill      = def
    }


----------------------------------------------------------------
-- Marker style
----------------------------------------------------------------

-- | Style of markers.
data MarkerParam = MarkerParam
  { _markerColor       :: Maybe (AlphaColour Double)
    -- ^ Color of marker
  , _markerBorderColor :: Maybe (AlphaColour Double)
  , _markerBorderWidth :: Double
  , _markerRadius      :: Double
  , _markerStyle       :: Maybe PointShape
  }

data LineParam = LineParam
  { _lineWidth  :: Double
  , _lineColor  :: Maybe (AlphaColour Double)
  , _lineDashes :: Maybe [Double]
  , _lineCap    :: LineCap
  , _lineJoin   :: LineJoin
  }

data FillParam = FillParam
  { _fillColor  :: !(Maybe (AlphaColour Double))
  , _fillEnable :: !Bool
  }

data BarplotParams = BarplotParams
  { _barplotZero    :: !Double
  , _barplotOutline :: !Bool
  }

instance Default MarkerParam where
  def = MarkerParam
    { _markerColor       = Nothing
    , _markerBorderColor = Nothing
    , _markerBorderWidth = 0
    , _markerRadius      = 1
    , _markerStyle       = Just PointShapeCircle
    }

instance Default LineParam where
  def = LineParam
    { _lineWidth  = 1
    , _lineColor  = Nothing
    , _lineDashes = Just []
    , _lineCap    = LineCapButt
    , _lineJoin   = LineJoinBevel
    }

instance Default FillParam where
  def = FillParam
    { _fillColor  = Nothing
    , _fillEnable = True
    }

instance Default BarplotParams where
  def = BarplotParams
    { _barplotZero    = 0
    , _barplotOutline = False
    }

$(makeLenses ''PlotParam)
$(makeLenses ''MarkerParam)
$(makeLenses ''LineParam)
$(makeLenses ''FillParam)
$(makeLenses ''BarplotParams)

----------------------------------------------------------------
-- Properties
--
-- We need lens definitions so 
----------------------------------------------------------------

----------------------------------------
-- PlotParam

instance (a ~ Double)      => IsLabel "color"  (Property (AlphaColour a) PlotParam) where
  fromLabel = Property plotMainColor

instance (p ~ MarkerParam) => IsLabel "marker" (Property p PlotParam) where
  fromLabel = Property plotMarker

instance (p ~ LineParam)   => IsLabel "line"   (Property p PlotParam) where
  fromLabel = Property plotLines

instance (p ~ FillParam)   => IsLabel "fill"   (Property p PlotParam) where
  fromLabel = Property plotFill


----------------------------------------
-- Marker

instance (p ~ PointShape) => IsLabel "shape" (Property (Maybe p) MarkerParam) where
  fromLabel = Property markerStyle

instance IsLabel "shape" (Property PointShape MarkerParam) where
  fromLabel = #shape . nonProp PointShapeCircle

instance a ~ Double => IsLabel "size" (Property a MarkerParam) where
  fromLabel = Property markerRadius

instance a ~ Double => IsLabel "borderwidth" (Property a MarkerParam) where
  fromLabel = Property markerBorderWidth

instance a ~ AlphaColour Double => IsLabel "bordercolor" (Property (Maybe a) MarkerParam) where
  fromLabel = Property markerBorderColor

instance a ~ Double => IsLabel "bordercolor" (Property (AlphaColour a) MarkerParam) where
  fromLabel = #bordercolor . nonProp (opaque black)

instance a ~ PointShape => IsLabel "style" (Property (Maybe a) MarkerParam) where
  fromLabel = Property markerStyle


----------------------------------------
-- Line

instance (p ~ [Double]) => IsLabel "shape" (Property (Maybe p) LineParam) where
  fromLabel = Property lineDashes


----------------------------------------
-- Fill

instance (p ~ AlphaColour Double) => IsLabel "color" (Property (Maybe p) FillParam) where
  fromLabel = Property fillColor

instance p ~ Double => IsLabel "color" (Property (AlphaColour p) FillParam) where
  fromLabel = #color . nonProp (opaque black)

instance (p ~ Bool) => IsLabel "enable" (Property p FillParam) where
  fromLabel = Property fillEnable



----------------------------------------
-- Barplot

instance (p ~ Double) => IsLabel "zero" (Property p BarplotParams) where
  fromLabel = Property barplotZero

instance (p ~ Bool) => IsLabel "outline" (Property p BarplotParams) where
  fromLabel = Property barplotOutline
