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



data PlotParam f = PlotParam
  { _plotMainColor :: f (AlphaColour Double) -- ^ Primary color of everything
  , _plotMarker    :: MarkerParam f
  }

data MarkerParam f = MarkerParam
  { _markerColor       :: f (Maybe (AlphaColour Double))
  , _markerBorderColor :: f (AlphaColour Double)
  , _markerBorderWidth :: f Double
  , _markerRadius      :: f Double
  , _markerStyle       :: f (Maybe PointShape)
  }

instance Default (PlotParam Identity) where
  def = PlotParam
    { _plotMainColor = Identity $ opaque black
    , _plotMarker    = def
    }

instance Default (MarkerParam Identity) where
  def = MarkerParam
    { _markerColor       = Identity $ Nothing
    , _markerBorderColor = Identity $ transparent
    , _markerBorderWidth = Identity 0
    , _markerRadius      = Identity 1
    , _markerStyle       = Identity (Just PointShapeCircle)
    }


instance Semigroup (PlotParam Endo) where
  p1 <> p2 = PlotParam
    { _plotMainColor = _plotMainColor p1 <> _plotMainColor p2
    , _plotMarker    = _plotMarker    p1 <> _plotMarker    p2
    }

instance Semigroup (MarkerParam Endo) where
  p1 <> p2 = MarkerParam
    { _markerColor       = _markerColor       p1 <> _markerColor       p2
    , _markerBorderColor = _markerBorderColor p1 <> _markerBorderColor p2
    , _markerBorderWidth = _markerBorderWidth p1 <> _markerBorderWidth p2
    , _markerRadius      = _markerRadius      p1 <> _markerRadius      p2
    , _markerStyle       = _markerStyle       p1 <> _markerStyle       p2
    }
  

instance Monoid (PlotParam Endo) where
  mempty = PlotParam
    { _plotMainColor = mempty
    , _plotMarker    = mempty
    }

instance Monoid (MarkerParam Endo) where
  mempty = MarkerParam
    { _markerColor       = mempty
    , _markerBorderColor = mempty
    , _markerBorderWidth = mempty
    , _markerRadius      = mempty
    , _markerStyle       = mempty
    }


class ApplyEndo p where
  applyEndo :: p Endo -> p Identity -> p Identity

instance ApplyEndo MarkerParam where
  applyEndo pEndo pDef = MarkerParam
    { _markerColor       = apply _markerColor
    , _markerBorderColor = apply _markerBorderColor
    , _markerBorderWidth = apply _markerBorderWidth
    , _markerRadius      = apply _markerRadius
    , _markerStyle       = apply _markerStyle
    }    
    where
      apply :: (forall f. MarkerParam f -> f a) -> Identity a
      apply f = Identity $ f pEndo `appEndo` runIdentity (f pDef)

instance ApplyEndo PlotParam where
  applyEndo pEndo pDef = PlotParam
    { _plotMainColor = apply _plotMainColor
    , _plotMarker    = applyEndo (_plotMarker pEndo) (_plotMarker pDef)
    }
    where
      apply :: (forall f. PlotParam f -> f a) -> Identity a
      apply f = Identity $ f pEndo `appEndo` runIdentity (f pDef)

endoL :: Setter' (Endo a) a
endoL fun (Endo f) = fmap Endo $ distribute $ fun . f

$(makeLenses ''PlotParam)
$(makeLenses ''MarkerParam)
