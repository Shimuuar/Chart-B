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
module Graphics.Rendering.ChartB where

import Data.Default.Class
import Data.Monoid
import Data.Coerce
import Data.Foldable
import Data.Maybe
import Data.Proxy
import Data.Ord
import Control.Arrow   ((***))
import Control.Category
import Control.Monad
import Control.Lens
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Prelude hiding (id,(.))
import Data.Colour
import Data.Colour.Names
import GHC.OverloadedLabels (IsLabel(..))

import qualified Graphics.Rendering.Chart.Axis.Floating as Axis

import Graphics.Rendering.ChartB.PlotParam
import Graphics.Rendering.ChartB.Class
import Graphics.Rendering.ChartB.Impl.Drawing
import Graphics.Rendering.ChartB.Impl.Axis

import Debug.Trace

save :: Renderable r -> IO ()
save = void . Cairo.renderableToFile
  (Cairo.FileOptions (800,600) Cairo.PNG)
  "q.png"



makePlot :: Plot Numeric Numeric -> IO ()
makePlot Plot{ plotObjects = (mconcat -> plt), ..} = save $ fillBackground def $ Renderable
  { minsize = return (0,0)
  , render  = \(w,h) -> do
      -- Viewport for plot itself
      let marginAxis = 0.03
          viewportTransform = Matrix
            { xx = (1 - 2*marginAxis)*w, yx = 0
            , xy = 0                   , yy = -(1 - 2*marginAxis)*h
            , x0 = w*marginAxis
            , y0 = h*(1-marginAxis)
            }
      -- Compute axes range
      let (rngX,rngY) = estimateRange (plotPointData plt) axisLimitX axisLimitY
          -- Merge range estimates
          (xA,xB) = fromRange rngX axisLimitX
          (yA,yB) = fromRange rngY axisLimitY
          dX      = xB - xA
          dY      = yB - yA
          plotTransform = Matrix
            { xx = 1/dX, yx = 0
            , xy = 0   , yy = 1/dY
            , x0 = -xA / dX
            , y0 = -yA / dY
            }
      let tr = plotTransform * viewportTransform
      withClipRegion (transformL viewportTransform $ Rect (Point 0 0) (Point 1 1))
        $ runDrawing tr $ plotFunction plt (plotParam plt)
      -- Plot axes on top of everything else
      alignStrokePoints [ transformL viewportTransform p
                        | p <- [Point 0 0, Point 0 1]
                        ] >>= strokePointPath
      alignStrokePoints [ transformL viewportTransform p
                        | p <- [Point 0 0, Point 1 0]
                        ] >>= strokePointPath
      -- Plot ticks
      let ticksX = map realToFrac $ steps 5 (xA,xB)
          ticksY = map realToFrac $ steps 5 (yA,yB)
      withLineStyle def $ do
        forM_ ticksX $ \x ->
          alignStrokePoints [ transformL tr p
                            | p <- [Point x yA, Point x (yA + 0.015*dY)]
                            ]
            >>= strokePointPath
        forM_ ticksY $ \y ->
          alignStrokePoints (let Point px py = transformL tr $ Point xA y
                             in [ Point px py, Point (px+5) py ]
                            )
            >>= strokePointPath
      withFontStyle def $ do
        forM_ ticksX $ \x ->
          drawTextA HTA_Centre VTA_Top (transformL tr (Point x yA)) (show x)
        forM_ ticksY $ \y ->
          drawTextA HTA_Right VTA_Centre (transformL tr (Point xA y)) (show y)
      return (const Nothing)
  }
  where
    fromRange _          (Just a , Just b)  = (a   , b  )
    fromRange UnknownLim (Nothing, Nothing) = (0   , 1  )
    fromRange UnknownLim (Just a,  Nothing) = (a   , a+1)
    fromRange UnknownLim (Nothing, Just b)  = (b-1 , b  )
    fromRange (MinMaxLimits a b) (Nothing, Nothing) = (a - 0.05*d, b + 0.05*d)
      where d = b - a
    fromRange (MinMaxLimits _ b) (Just a, Nothing) = (a, b + 0.05*d)
      where b' = max a b
            d  = b' - a
    fromRange (MinMaxLimits a _) (Nothing, Just b) = (a' - 0.05*d, b)
      where a' = min a b
            d  = b - a'



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

scatterplotRender :: Fold s (Double,Double) -> s -> Endo PlotParam -> Drawing ()
scatterplotRender optic xy pEndo = do
  advanceColorWheel
  p <- appEndo pEndo <$> getDefaultPlotParam
  -- Draw lines
  --
  -- FIXME: do not materialize list
  usingLineStype p $ \style ->
    liftedDrawLines style $ xy ^.. optic . to (uncurry Point)
  -- Draw markers
  usingPointStype p $ \style ->
    forMOf_ optic xy $ \(x,y) -> do
      liftedDrawPoint style $ Point x y



data Bar = Bar Double Double Double

barplotOf :: Fold s Bar -> s -> PlotObj Numeric Numeric
barplotOf optic bars = PlotObj
  { plotFunction  = \pEndo -> do
      advanceColorWheel
      p <- appEndo pEndo <$> getDefaultPlotParam
      usingLineStype p $ \style ->
        forMOf_ optic bars $ \(Bar x1 x2 y) ->
          liftedDrawLines style $ [ Point x1 0, Point x1 y, Point x2 y, Point x2 0 ]
  --
  , plotPointData = FoldOverAxes $ \_ stepX stepY a0 ->
        flip stepY 0
      $ foldlOf' optic (\a (Bar x1 x2 y) -> flip stepY y $ flip stepX x2 $ flip stepX x1 a) a0 bars
  --
  , plotParam     = mempty
  }

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

----------------------------------------------------------------
--
----------------------------------------------------------------

x2,x3 :: [(Double,Double)]
x2 = [(x,x*x)   | x <- [0.3, 0.31 .. 1 ]]
x3 = [(x,x*x*x) | x <- [0.3, 0.31 .. 1 ]]
xs :: [Double]
xs = [0.3, 0.31 .. 1.1 ]

-- go = plot
--   [ scatterplot x2
--     & prop (#marker . #size)        .~ 4
--     & prop (#marker . #shape)       .~ Just PointShapeStar
--     & prop (#marker . #borderwidth) .~ 1
--     -- & prop #marker . markerBorderColor .~ Just (opaque green)
--   , lineplot x3
--   ]
--   & prop #xlim .~ (Nothing, Just 1)

go2 = plot
  [ scatterplotOf each x2
  , scatterplotOf each x3
  , scatterplotOf (each . to (\x -> (x,x**2.2))) xs
  , barplotOf each
    [ Bar 0.0 0.2 0.3
    , Bar 0.4 0.5 (-0.2)
    ]
  ]

-- go3 = plot
--   [ (scatterplot x2 <> lineplot x3)
--   & prop #color .~ opaque green
--   ]
--   & prop #xlim .~ (Nothing, Just 1)



----------------------------------------------------------------
-- Properties
----------------------------------------------------------------

newtype Property a s = Property { prop :: Setter' s a }

instance Category Property where
  id = id
  Property f . Property g = Property (f . g)


instance IsLabel l (Property p a) => IsLabel l (Property p (Endo a)) where
  fromLabel = Property endoL . fromLabel @l


----------------------------------------
-- Plot axes properties

instance ( AxisValue x ~ xlim, AxisValue x ~ xlim'
         ) => IsLabel "xlim" (Property (Maybe xlim, Maybe xlim') (Plot x y)) where
  fromLabel = Property $ lens axisLimitX (\p x -> p { axisLimitX = x })

instance ( AxisValue y ~ ylim, AxisValue y ~ ylim'
         ) => IsLabel "ylim" (Property (Maybe ylim, Maybe ylim') (Plot x y)) where
  fromLabel = Property $ lens axisLimitY (\p x -> p { axisLimitY = x })


----------------------------------------
-- Plot object properties

instance IsLabel l (Property p PlotParam) => IsLabel l  (Property p (PlotObj x y)) where
  fromLabel = Property (lens plotParam (\x p -> x { plotParam = p }))
            . fromLabel @l


instance (a ~ Double)      => IsLabel "color"  (Property (AlphaColour a) PlotParam) where
  fromLabel = Property plotMainColor

instance (p ~ MarkerParam) => IsLabel "marker" (Property p PlotParam) where
  fromLabel = Property plotMarker

instance (p ~ LineParam)   => IsLabel "line"   (Property p PlotParam) where
  fromLabel = Property plotLines



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

nonProp :: a -> Property a (Maybe a)
nonProp x0 = Property $ \f -> fmap Just . f . fromMaybe x0


----------------------------------------------------------------
-- Plot objects
----------------------------------------------------------------

plot :: [PlotObj x y] -> Plot x y
plot ps = mempty { plotObjects = ps }


-- | Complete plot with single pait of axes
data Plot x y = Plot
  { plotObjects :: [PlotObj x y]
  , axisLimitX  :: (Maybe (AxisValue x), Maybe (AxisValue x))
  , axisLimitY  :: (Maybe (AxisValue y), Maybe (AxisValue y))
  }


-- | Single entity on plog
data PlotObj x y = PlotObj
  { plotFunction  :: Endo PlotParam -> Drawing ()
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
    , axisLimitX = axisLimitX a `onFirst` axisLimitX b
    , axisLimitY = axisLimitY a `onFirst` axisLimitY b
    }
    where
      onFirst :: forall a. (Maybe a, Maybe a) -> (Maybe a, Maybe a) -> (Maybe a, Maybe a)
      onFirst x y = coerce (coerce x <> coerce y :: (First a, First a))

instance Monoid (Plot x y) where
  mempty = Plot
    { plotObjects = []
    , axisLimitX  = (Nothing,Nothing)
    , axisLimitY  = (Nothing,Nothing)
    }

----------------------------------------------------------------
-- Axes
----------------------------------------------------------------


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
