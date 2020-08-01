{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
module Graphics.Rendering.ChartB.PlotParam where

import Data.Default.Class
import Data.Monoid
import Data.Distributive
import Control.Lens
import Graphics.Rendering.Chart.Drawing
import Data.Colour



data PlotParam = PlotParam
  { _plotMainColor :: !(AlphaColour Double) -- ^ Primary color of everything
  , _plotMarker    :: !MarkerParam
  , _plotLines     :: !LineParam
  , _plotFill      :: !FillParam
  }

data MarkerParam = MarkerParam
  { _markerColor       :: Maybe (AlphaColour Double)
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


instance Default PlotParam where
  def = PlotParam
    { _plotMainColor = opaque black
    , _plotMarker    = def
    , _plotLines     = def
    , _plotFill      = def
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
