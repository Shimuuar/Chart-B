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
import Control.Applicative
import Control.Monad
import Control.Lens
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Debug.Trace

import Data.Colour
import Data.Colour.Names
import GHC.OverloadedLabels (IsLabel(..))



data PlotParam f = PlotParam
  { _plotMainColor :: f (Colour Double)
  , _plotMainAlpha :: f (Double)
  }

instance Semigroup (PlotParam Endo) where
  p1 <> p2 = PlotParam
    { _plotMainColor = _plotMainColor p1 <> _plotMainColor p2
    , _plotMainAlpha = _plotMainAlpha p1 <> _plotMainAlpha p2
    }

instance Monoid (PlotParam Endo) where
  mempty = PlotParam
    { _plotMainColor = mempty
    , _plotMainAlpha = mempty
    }


applyPlotParam :: PlotParam Endo -> PlotParam Identity -> PlotParam Identity
applyPlotParam pEndo pDef = PlotParam
  { _plotMainColor = apply _plotMainColor
  , _plotMainAlpha = apply _plotMainAlpha
  }
  where
    apply :: (forall f. PlotParam f -> f a) -> Identity a
    apply f = Identity $ f pEndo `appEndo` runIdentity (f pDef)

endoL :: Setter' (Endo a) a
endoL fun (Endo f) = fmap Endo $ distribute $ fun . f

$(makeLenses ''PlotParam)
