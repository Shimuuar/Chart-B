{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
-- |
module Graphics.Rendering.ChartB where

import Data.Default.Class
import Control.Monad
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Debug.Trace

save :: Renderable r -> IO ()
save = void . Cairo.renderableToFile
  (Cairo.FileOptions (800,600) Cairo.PNG)
  "q.png"



go plt = save $ fillBackground def $ Renderable
  { minsize = return (0,0)
  , render  = \(w,h) -> do
      let transform = Matrix
            { xx = 0.9*w, yx = 0
            , xy = 0    , yy = -0.9*h
            , x0 = w*0.05
            , y0 = h*0.95
            }
      plot plt (param plt) transform
      return (const Nothing)
  }

test :: PlotObj Numeric Numeric
test = PlotObj
  { plot  = \_ tr -> do
      forM_ [0, 1e-3 .. 1] $ \x ->
        drawPoint def $ transformP tr $ Point x (x*x)
      return ()
  , axisX = ()
  , axisY = ()
  , param = PlotParam
  }

----------------------------------------------------------------
--
----------------------------------------------------------------

data PlotParam = PlotParam


-- | Single entity on plog
data PlotObj x y = PlotObj
  { plot  :: PlotParam -> Matrix -> BackendProgram ()
  , axisX :: Acc x
  , axisY :: Acc y
  , param :: PlotParam
  }



----------------------------------------------------------------
-- Axes
----------------------------------------------------------------

class Monoid (Acc a) => Axis a where
  type Acc a


data Numeric

instance Axis Numeric where
  type Acc Numeric = ()

data Time
data Categori
