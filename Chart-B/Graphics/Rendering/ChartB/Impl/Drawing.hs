{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
module Graphics.Rendering.ChartB.Impl.Drawing where

import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.Operational
import Data.Default.Class
import Data.Colour
import Data.Colour.Names
import Data.Monoid
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry

import Graphics.Rendering.ChartB.Class
import Graphics.Rendering.ChartB.Types.PlotParam
import Graphics.Rendering.ChartB.Types.Axis
import Graphics.Rendering.Chart.Backend.Impl

----------------------------------------------------------------
-- Drawing monad
----------------------------------------------------------------

-- | Wrapper on top of BackendProgram for drawing. It keeps track of
--   current viewport transformation and color wheel.
newtype Drawing x y a = Drawing
  { unDrawing :: StateT [AlphaColour Double]
      (ReaderT (Matrix, AxisValue x -> Double, AxisValue y -> Double) (Program ChartBackendInstr)) a }
  deriving newtype (Functor, Applicative, Monad)

-- | Execute drawing program
runDrawing
  :: Matrix
  -> (AxisValue x -> Double)
  -> (AxisValue y -> Double)
  -> Drawing x y a
  -> BackendProgram a
runDrawing tr viewX viewY (Drawing act)
  = flip runReaderT (tr, viewX, viewY)
  $ evalStateT act defColors
  where
    -- Since each plot advances color wheel before we start plotting
    -- we have to add dummy value in from
    defColors = opaque blue
              : cycle (map opaque $ [blue, red, green, orange, cyan, magenta])

newPlot :: Endo PlotParam -> Drawing x y PlotParam
newPlot pEndo = do
  advanceColorWheel
  appEndo pEndo <$> getDefaultPlotParam

advanceColorWheel :: Drawing x y ()
advanceColorWheel = Drawing $ modify tail

getDefaultPlotParam :: Drawing x y PlotParam
getDefaultPlotParam = Drawing $ do
  c <- head <$> get
  return $ def & plotMainColor .~ c

liftedDrawPoint :: PointStyle -> (AxisValue x, AxisValue y) -> Drawing x y ()
liftedDrawPoint st (x,y) = Drawing $ do
  (tr,fx,fy) <- ask
  lift $ lift $ drawPoint st (transformL tr $ Point (fx x) (fy y))
  

liftedDrawLines :: LineStyle -> [(AxisValue x, AxisValue y)] -> Drawing x y ()
liftedDrawLines style pts = Drawing $ do
  (tr,fx,fy) <- ask
  lift $ lift $ withLineStyle style $ strokePointPath
    [ transformL tr $ Point (fx x) (fy y) | (x,y) <- pts ]

liftedDrawAlignedLines :: LineStyle -> [(AxisValue x, AxisValue y)] -> Drawing x y ()
liftedDrawAlignedLines style pts = Drawing $ do
  (tr,fx,fy) <- ask
  lift $ lift $ withLineStyle style $ strokeAlignedPointPath
    [ transformL tr $ Point (fx x) (fy y) | (x,y) <- pts ]

liftedFillPath :: FillStyle -> [(AxisValue x, AxisValue y)] -> Drawing x y ()
liftedFillPath style pts = Drawing $ do
  (tr,fx,fy) <- ask
  lift $ lift $ withFillStyle style $ fillPointPath 
    [ transformL tr $ Point (fx x) (fy y) | (x,y) <- pts ]

strokeAlignedPointPath :: [Point] -> BackendProgram ()
strokeAlignedPointPath = strokePointPath <=< alignStrokePoints
