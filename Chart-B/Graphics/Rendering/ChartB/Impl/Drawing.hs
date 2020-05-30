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
import Graphics.Rendering.ChartB.PlotParam
import Graphics.Rendering.Chart.Backend
import Graphics.Rendering.Chart.Backend.Impl

----------------------------------------------------------------
-- Drawing monad
----------------------------------------------------------------

-- | Wrapper on top of BackendProgram for drawing. It keeps track of
--   current viewport transformation and color wheel.
newtype Drawing a = Drawing (StateT [AlphaColour Double] (ReaderT Matrix (Program ChartBackendInstr)) a)
  deriving newtype (Functor, Applicative, Monad)

-- | Execute drawing program
runDrawing :: Matrix -> Drawing a -> BackendProgram a
runDrawing tr (Drawing act)
  = flip runReaderT tr
  $ evalStateT act defColors
  where
    -- Since each plot advances color wheel before we start plotting
    -- we have to add dummy value in from
    defColors = opaque blue
              : cycle (map opaque $ [blue, red, green, orange, cyan, magenta])

newPlot :: Endo PlotParam -> Drawing PlotParam
newPlot pEndo = do
  advanceColorWheel
  appEndo pEndo <$> getDefaultPlotParam

advanceColorWheel :: Drawing ()
advanceColorWheel = Drawing $ modify tail

getDefaultPlotParam :: Drawing PlotParam
getDefaultPlotParam = Drawing $ do
  c <- head <$> get
  return $ def & plotMainColor .~ c

liftedDrawPoint :: PointStyle -> Point -> Drawing ()
liftedDrawPoint st p = do
  tr <- Drawing ask
  Drawing $ lift $ lift $ drawPoint st (transformL tr p)

liftedDrawLines :: LineStyle -> [Point] -> Drawing ()
liftedDrawLines style pts = Drawing $ do
  tr <- ask
  lift $ lift $ withLineStyle style $ strokePointPath $ transformL tr <$> pts

liftedFillPath :: FillStyle -> [Point] -> Drawing ()
liftedFillPath style pts = Drawing $ do
  tr <- ask
  lift $ lift $ withFillStyle style $ fillPointPath $ transformL tr <$> pts

strokeAlignedPointPath :: [Point] -> BackendProgram ()
strokeAlignedPointPath = strokePointPath <=< alignStrokePoints
