{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
-- |
module Graphics.Rendering.ChartB
  ( -- * Creating plots
    plotToRenderable
  , plot
  , Numeric
  , Property(..)
    -- ** Indivudal plot types
  , scatterplotOf
  , lineplotOf
  , barplotOf
    -- * Underlying data types
  , Plot(..)
  , PlotObj(..)
  ) where

import Data.Default.Class
import Data.Monoid
import Data.Foldable
import Data.Maybe
import Control.Arrow   ((***))
import Control.Category
import Control.Monad
import Control.Lens
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Prelude hiding (id,(.))
import Data.Colour
import Data.Colour.Names
import GHC.OverloadedLabels (IsLabel(..))

import Graphics.Rendering.ChartB.Class
import Graphics.Rendering.ChartB.Impl.Drawing
import Graphics.Rendering.ChartB.Types.Axis
import Graphics.Rendering.ChartB.Types.PlotParam
import Graphics.Rendering.ChartB.Types.Property


----------------------------------------------------------------
-- Rendering of plots
----------------------------------------------------------------

plotToRenderable :: (Axis x, Axis y) => Plot x y -> Renderable ()
plotToRenderable Plot{ plotObjects = (mconcat -> plt), ..} = Renderable
  { minsize = return (0,0)
  , render  = \(w,h) -> do
      -- First we need to compute transform for viewport of the plot
      -- (area where we do all the plotting). This requires to compute
      -- labels for plot and allocate size for them

      -- First we need to compute ranges for the plot and transform
      -- from plot coordinates to viewport coordinates
      let (rngX,rngY) = estimateRange (plotPointData plt) axisX axisY
          axisTrX = makeAxisTransform axisX rngX
          axisTrY = makeAxisTransform axisY rngY
          (xA,xB) = axisPlotRange axisTrX
          (yA,yB) = axisPlotRange axisTrY
          dX      = xB - xA
          dY      = yB - yA
          plotTransform = Matrix
            { xx = 1/dX, yx = 0
            , xy = 0   , yy = 1/dY
            , x0 = -xA / dX
            , y0 = -yA / dY
            }
      -- Now we need to compute labels for axes, margins for labels
      let ticksX = axisTicks axisTrX
          ticksY = axisTicks axisTrY
          funX   = axisPointMap axisTrX
          funY   = axisPointMap axisTrY
      ViewportLayout{..} <- computeViewportLayout (w,h) plotTitle ticksX ticksY
      let tr = plotTransform * viewportTransform
      -- Draw plots
      withClipRegion (transformL viewportTransform $ Rect (Point 0 0) (Point 1 1)) $
        runDrawing tr funX funY $ do
          when plotGrid $ do
            let gridStyle = def & line_color .~ opaque lightgray
            forM_ ticksX $ \(Tick _ (funX -> x)) -> do
              liftedDrawAlignedLinesU gridStyle [(x,yA), (x,yB)]
            forM_ ticksY $ \(Tick _ (funY -> y)) -> do
              liftedDrawAlignedLinesU gridStyle [(xA,y), (xB,y)]
          plotFunction plt (plotParam plt)
      -- Plot axes on top of everything else
      strokeAlignedPointPath $ transformL viewportTransform <$> [Point 0 0, Point 0 1]
      strokeAlignedPointPath $ transformL viewportTransform <$> [Point 0 0, Point 1 0]
      withLineStyle def $ do
        forM_ ticksX $ \(Tick _ x) -> do
          let Point px py = transformL tr $ Point (funX x) yA
          strokeAlignedPointPath [Point px py, Point px (py-5)]
        forM_ ticksY $ \(Tick _ y) -> do
          let Point px py = transformL tr $ Point xA (funY y)
          strokeAlignedPointPath [ Point px py, Point (px+5) py ]
      -- Plot labels
      withFontStyle def $ do
        forM_ ticksX $ \(Tick nm x) -> do
          let Point x' y' = transformL tr $ Point (funX x) yA
          drawTextA HTA_Centre VTA_Top (Point x' (y'+2)) nm
        forM_ ticksY $ \(Tick nm y) -> do
          let Point x' y' = transformL tr $ Point xA (funY y)
          drawTextA HTA_Right VTA_Centre (Point (x'-marginAxis) y') nm
      -- Plot title
      forM_ plotTitle $ \title -> do
        let p = transformL viewportTransform $ Point 0.5 1
        drawTextA HTA_Centre VTA_Bottom p title
      return (const Nothing)
  }


data ViewportLayout = ViewportLayout
  { marginAxis        :: !Double
  , labelMarginX      :: !Double
  , labelMarginY      :: !Double
  , viewportTransform :: !Matrix
  }

computeViewportLayout :: (Double,Double) -> Maybe String -> [Tick x] -> [Tick y] -> BackendProgram ViewportLayout
computeViewportLayout (w,h) title ticksX ticksY = do
  labelMarginX <- case ticksX of
    [] -> return 0
    _  -> maximum . map fst <$> mapM (textDimension . tickLabel) ticksY
  labelMarginY <- case ticksY of
    [] -> return 0
    _  -> maximum . map snd <$> mapM (textDimension . tickLabel) ticksX
  titleMarginY <- case title of
    Nothing -> return 0
    Just "" -> return 0
    Just  s -> snd <$> textDimension s
  return ViewportLayout
    { viewportTransform = Matrix
        { xx =  (w - marginAxis*3 - labelMarginX)
        , yy = -(h - marginAxis*3 - labelMarginY - titleMarginY)
        , yx = 0
        , xy = 0
        , x0 = marginAxis * 2 + labelMarginX
        , y0 = h - (marginAxis * 2 + labelMarginY)
        }
    , ..
    }
  where
    marginAxis = 5



----------------------------------------------------------------
-- Concrete plot objects
----------------------------------------------------------------

scatterplotOf :: (Real a, Real b) => Fold s (a,b) -> s -> PlotObj Numeric Numeric
{-# INLINE scatterplotOf #-}
scatterplotOf optic xy = PlotObj
  { plotFunction  = scatterplotRender (optic . to (realToFrac *** realToFrac)) xy
  , plotPointData = FoldOverAxes $ \stepXY _ _ a0 ->
      foldlOf' optic (\a (x,y) -> stepXY a (realToFrac x) (realToFrac y)) a0 xy
  , plotParam     = mempty
                  & prop (#line . #shape) .~ Nothing
  }

lineplotOf :: (Real a, Real b) => Fold s (a,b) -> s -> PlotObj Numeric Numeric
lineplotOf optic xy = PlotObj
  { plotFunction  = scatterplotRender (optic . to (realToFrac *** realToFrac)) xy
  , plotPointData = FoldOverAxes $ \stepXY _ _ a0 ->
      foldlOf' optic (\a (x,y) -> stepXY a (realToFrac x) (realToFrac y)) a0 xy
  , plotParam     = mempty
                  & prop (#marker . #shape) .~ Nothing
  }

scatterplotRender :: Fold s (Double,Double) -> s -> Endo PlotParam -> Drawing Numeric Numeric ()
scatterplotRender optic xy = newPlot >=> \p -> do
  -- Draw lines
  --
  -- FIXME: do not materialize list
  usingLineStype p $ \style ->
    liftedDrawLines style $ xy ^.. optic
  -- Draw markers
  usingPointStype p $ \style ->
    forMOf_ optic xy $ liftedDrawPoint style






barplotOf
  :: (Real x, Real y)
  => [BarplotParams -> BarplotParams]
  -> Fold s (x,y)
  -> s
  -> PlotObj Numeric Numeric
barplotOf params optic xy = PlotObj
  { plotFunction = newPlot >=> plotFunctionImpl
  --
  , plotPointData = FoldOverAxes $ \stepXY stepX stepY a0 ->
      case param ^. barplotOutline of
        True  -> foldl' (\a (x,y) -> stepXY a x y) a0 asOutline
        False -> flip stepY zero
               $ foldl' (\a (Bar x1 x2 y) -> flip stepY y $ flip stepX x2 $ flip stepX x1 a) a0 asBars
  --
  , plotParam     = mempty
  }
  where
    plotFunctionImpl
      | param^.barplotOutline = outlineBarplot
      | otherwise             = normalBarplot
    --
    normalBarplot p = do
      usingFillStype p $ \style ->
        forM_ asBars $ \(Bar x1 x2 y) ->
          liftedFillPath style $ [ (x1, zero), (x1, y), (x2, y), (x2, zero) ]
      usingLineStype p $ \style ->
        forM_ asBars $ \(Bar x1 x2 y) ->
          liftedDrawLines style $ [ (x1, zero), (x1, y), (x2, y), (x2, zero) ]
    --
    outlineBarplot p = do
      usingFillStype p $ \style ->
        liftedFillPath style asOutline
      usingLineStype p $ \style ->
        liftedDrawLines style asOutline
    --
    asBars    = toBars         $ xy ^.. optic . to (realToFrac *** realToFrac)
    asOutline = toOutline zero $ xy ^.. optic . to (realToFrac *** realToFrac)
    --
    param  = appEndo (foldMap Endo params) def
    zero   = param ^. barplotZero


toBars :: Fractional x => [(x, y)] -> [Bar x y]
toBars = transformAdjacent
      ( \(x,y) -> (Bar (x-0.5) (x+0.5) y))
      ( \(x,y)  (xB,_)        -> let dx = (xB - x)/2 in Bar (x-dx) (x+dx) y
      , \(xA,_) (x,y)  (xB,_) -> Bar ((x+xA)/2) ((x+xB)/2) y
      , \       (xA,_) (x,y)  -> let dx = (x - xA)/2 in Bar (x-dx) (x+dx) y
      )

toOutline :: Double -> [(Double, Double)] -> [(Double,Double)]
toOutline zero = concat . transformAdjacent
      ( \(x,y) -> [ (x-0.5, zero)
                  , (x-0.5, y)
                  , (x+0.5, y)
                  , (x+0.5, zero)
                  ])
      ( \(x,y)  (xB,_)        -> let dX = (xB - x)/2 in
                                   [ (x-dX, zero)
                                   , (x-dX, y)
                                   , (x+dX, y)
                                   ]
      , \(xA,yA) (x,y)  (xB,_) ->
          [ ((xA+x)/2, yA)
          , ((xA+x)/2, y)
          , ((x+xB)/2, y)
          ]
      , \(xA,yA) (x,y)  -> let dX = (x - xA)/2 in
                             [ (x-dX, yA)
                             , (x-dX, y)
                             , (x+dX, y)
                             , (x+dX, zero)
                             ]
      )


transformAdjacent
  :: (a -> b)
  -> ((a -> a -> b), (a -> a -> a -> b), (a -> a -> b))
  -> [a]
  -> [b]
transformAdjacent _ _ []  = []
transformAdjacent f _ [a] = [f a]
transformAdjacent _ (fa,_,fb) [a1,a2] = [fa a1 a2, fb a1 a2]
transformAdjacent _ (fa,f,fb) xs@(a1:a2:_) = fa a1 a2 : go xs
  where
    go (a:as@(b:c:_)) = f a b c : go as
    go [a,b]          = [fb a b]
    go _              = error "transformAdjacent: impossible"

data Bar x y = Bar !x !x !y
  deriving Show


usingPointStype :: Monad m => PlotParam -> (PointStyle -> m ()) -> m ()
usingPointStype p action = mapM_ action $ do
  s <- p ^. plotMarker . markerStyle
  Just PointStyle
    { _point_color        = fromMaybe (p ^. plotMainColor)
                          $ p ^. plotMarker . markerColor
    , _point_border_color = fromMaybe (p ^. plotMainColor)
                          $ p ^. plotMarker . markerBorderColor
    , _point_border_width = p ^. plotMarker . markerBorderWidth
    , _point_radius       = p ^. plotMarker . markerRadius
    , _point_shape        = s
    }

usingLineStype :: Monad m => PlotParam -> (LineStyle -> m ()) -> m ()
usingLineStype p action = mapM_ action $ do
  s <- p ^. plotLines . lineDashes
  Just LineStyle
    { _line_color  = fromMaybe (p ^. plotMainColor)
                   $ p ^. plotLines . lineColor
    , _line_width  = p ^. plotLines . lineWidth
    , _line_dashes = s
    , _line_cap    = p ^. plotLines . lineCap
    , _line_join   = p ^. plotLines . lineJoin
    }

usingFillStype :: Monad m => PlotParam -> (FillStyle -> m ()) -> m ()
usingFillStype p action = do
  when (p ^. plotFill . fillEnable) $ do
    action FillStyleSolid
      { _fill_color = fromMaybe (blend 0.5 (opaque white) (p ^. plotMainColor))
                    $ (p ^. plotFill . fillColor)
      }



----------------------------------------------------------------
-- Properties
----------------------------------------------------------------



----------------------------------------
-- Plot properties

instance ( AxisValue x ~ xlim, AxisValue x ~ xlim'
         ) => IsLabel "xlim" (Property (Maybe xlim, Maybe xlim') (Plot x y)) where
  fromLabel = #xaxis . #lim

instance ( AxisValue y ~ ylim, AxisValue y ~ ylim'
         ) => IsLabel "ylim" (Property (Maybe ylim, Maybe ylim') (Plot x y)) where
  fromLabel = #yaxis . #lim

instance IsLabel "title" (Property [Char] (Plot x y)) where
  fromLabel = Property (lens plotTitle (\p x -> p { plotTitle = x }))
            . nonProp ""

instance IsLabel "title" (Property (Maybe [Char]) (Plot x y)) where
  fromLabel = Property (lens plotTitle (\p x -> p { plotTitle = x }))

instance IsLabel "grid" (Property Bool (Plot x y)) where
  fromLabel = Property (lens plotGrid (\p x -> p { plotGrid = x }))

instance (AxisParam x ~ a) => IsLabel "xaxis" (Property a (Plot x y)) where
  fromLabel = Property (lens axisX (\p x -> p { axisX = x }))

instance (AxisParam y ~ a) => IsLabel "yaxis" (Property a (Plot x y)) where
  fromLabel = Property (lens axisY (\p x -> p { axisY = x }))


instance IsLabel "logx" (Property Bool (Plot x y)) where
  fromLabel = #yaxis . #log

instance IsLabel "logy" (Property Bool (Plot x y)) where
  fromLabel = #xaxis . #log


----------------------------------------
-- Plot object properties

instance IsLabel l (Property p PlotParam) => IsLabel l  (Property p (PlotObj x y)) where
  fromLabel = Property (lens plotParam (\x p -> x { plotParam = p }))
            . fromLabel @l



----------------------------------------------------------------
-- Plot objects
----------------------------------------------------------------

plot :: [PlotObj x y] -> Plot x y
plot ps = mempty { plotObjects = ps }


-- | Complete plot with single pait of axes
data Plot x y = Plot
  { plotObjects :: [PlotObj x y]
  , axisX       :: !(AxisParam x)
  , axisY       :: !(AxisParam y)
  , plotTitle   :: !(Maybe String)
  , plotGrid    :: !Bool
  }

-- | Single entity on plog
data PlotObj x y = PlotObj
  { plotFunction  :: Endo PlotParam -> Drawing x y ()
  , plotPointData :: FoldOverAxes x y
  , plotParam     :: Endo PlotParam
  }

instance (Axis x, Axis y) => Semigroup (PlotObj x y) where
  a <> b = PlotObj
    { plotFunction  = \p -> do plotFunction a (p <> plotParam a)
                               plotFunction b (p <> plotParam b)
    , plotPointData = plotPointData a <> plotPointData b
    , plotParam     = mempty
    }

instance (Axis x, Axis y) => Monoid (PlotObj x y) where
  mempty = PlotObj
    { plotFunction  = \_ -> return ()
    , plotPointData = FoldOverAxes $ \_ _ _ -> id
    , plotParam     = mempty
    }

instance Semigroup (Plot x y) where
  a <> b = Plot
    { plotObjects = plotObjects a <> plotObjects b
    , axisX       = axisX a <> axisX b
    , axisY       = axisY a <> axisY b
    , plotTitle   = getFirst $ First (plotTitle  a) <> First (plotTitle b)
    , plotGrid    = plotGrid a || plotGrid b
    }

instance Monoid (Plot x y) where
  mempty = Plot
    { plotObjects = []
    , axisX       = mempty
    , axisY       = mempty
    , plotTitle   = mempty
    , plotGrid    = False
    }

----------------------------------------------------------------
-- Axes
----------------------------------------------------------------

