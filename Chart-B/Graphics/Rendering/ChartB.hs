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
  , prop
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
import Data.Coerce
import Data.Foldable
import Data.Maybe
import Data.Ord
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


import Graphics.Rendering.ChartB.PlotParam
import Graphics.Rendering.ChartB.Class
import Graphics.Rendering.ChartB.Impl.Drawing
import Graphics.Rendering.ChartB.Impl.Axis


----------------------------------------------------------------
-- Rendering of plots
----------------------------------------------------------------

plotToRenderable :: Plot Numeric Numeric -> Renderable ()
plotToRenderable Plot{ plotObjects = (mconcat -> plt), ..} = Renderable
  { minsize = return (0,0)
  , render  = \(w,h) -> do
      -- First we need to compute transform for viewport of the plot
      -- (area where we do all the plotting). This requires to compute
      -- labels for plot and allocate size for them

      -- First we need to compute ranges for the plot and transform
      -- from plot coordinates to viewport coordinates
      let (rngX,rngY) = estimateRange (plotPointData plt) axisLimitX axisLimitY
          (xA,xB)     = fromRange rngX axisLimitX
          (yA,yB)     = fromRange rngY axisLimitY
          dX          = xB - xA
          dY          = yB - yA
          plotTransform = Matrix
            { xx = 1/dX, yx = 0
            , xy = 0   , yy = 1/dY
            , x0 = -xA / dX
            , y0 = -yA / dY
            }
      -- Now we need to compute labels for axes, margins for labels
      let ticksX = map realToFrac $ steps 5 (xA,xB)
          ticksY = map realToFrac $ steps 5 (yA,yB)
      labelMarginX <-  maximum . map fst
                   <$> mapM (textDimension . show) ticksY
      labelMarginY <-  maximum . map snd
                   <$> mapM (textDimension . show) ticksX
      titleMarginY <- case plotTitle of
        Nothing -> return 0
        Just "" -> return 0
        Just  s -> snd <$> textDimension s
      -- Compute
      let marginAxis = 5
          viewportTransform = Matrix
            { xx =  (w - marginAxis*3 - labelMarginX)
            , yy = -(h - marginAxis*3 - labelMarginY - titleMarginY)
            , yx = 0
            , xy = 0
            , x0 = marginAxis * 2 + labelMarginX
            , y0 = h - (marginAxis * 2 + labelMarginY)
            }
      let tr = plotTransform * viewportTransform
      -- Draw grid
      when plotGrid $ do
        let gridStyle = def & line_color .~ opaque lightgray
        withLineStyle gridStyle $ do
          forM_ ticksX $ \x -> do
            let p1 = transformL tr $ Point x yA
                p2 = transformL tr $ Point x yB
            strokeAlignedPointPath [p1, p2]
          forM_ ticksY $ \y -> do
            let p1 = transformL tr $ Point xA y
                p2 = transformL tr $ Point xB y
            strokeAlignedPointPath [p1, p2]
      -- Draw plots
      withClipRegion (transformL viewportTransform $ Rect (Point 0 0) (Point 1 1))
        $ runDrawing tr $ plotFunction plt (plotParam plt)
      -- Plot axes on top of everything else
      strokeAlignedPointPath $ transformL viewportTransform <$> [Point 0 0, Point 0 1]
      strokeAlignedPointPath $ transformL viewportTransform <$> [Point 0 0, Point 1 0]
      withLineStyle def $ do
        forM_ ticksX $ \x -> do
          let Point px py = transformL tr $ Point x yA
          strokeAlignedPointPath [Point px py, Point px (py-5)]
        forM_ ticksY $ \y -> do
          let Point px py = transformL tr $ Point xA y
          strokeAlignedPointPath [ Point px py, Point (px+5) py ]
      -- Plot labels
      withFontStyle def $ do
        forM_ ticksX $ \x -> do
          let Point x' y' = transformL tr $ Point x yA
          drawTextA HTA_Centre VTA_Top (Point x' (y'+2)) (show x)
        forM_ ticksY $ \y -> do
          let Point x' y' = transformL tr $ Point xA y
          drawTextA HTA_Right VTA_Centre (Point (x'-marginAxis) y') (show y)
      -- Plot title
      forM_ plotTitle $ \title -> do
        let p = transformL viewportTransform $ Point 0.5 1 
        drawTextA HTA_Centre VTA_Bottom p title
      return (const Nothing)
  }
  where

fromRange :: AxisRangeEst Numeric -> (Maybe Double, Maybe Double) -> (Double, Double)
fromRange _          (Just a , Just b)  = (a   , b  )
fromRange UnknownLim (Nothing, Nothing) = (0   , 1  )
fromRange UnknownLim (Just a,  Nothing) = (a   , a+1)
fromRange UnknownLim (Nothing, Just b)  = (b-1 , b  )
fromRange (MinMaxLimits a b) (Nothing, Nothing)
  | a == b    = (a - 0.5, a + 0.5)
  | otherwise = (a - 0.05*d, b + 0.05*d)
  where d = b - a
fromRange (MinMaxLimits _ b) (Just a, Nothing) = (a, b + 0.05*d)
  where b' = max a b
        d  = b' - a
fromRange (MinMaxLimits a _) (Nothing, Just b) = (a' - 0.05*d, b)
  where a' = min a b
        d  = b - a'



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

scatterplotRender :: Fold s (Double,Double) -> s -> Endo PlotParam -> Drawing ()
scatterplotRender optic xy = newPlot >=> \p -> do
  -- Draw lines
  --
  -- FIXME: do not materialize list
  usingLineStype p $ \style ->
    liftedDrawLines style $ xy ^.. optic . to (uncurry Point)
  -- Draw markers
  usingPointStype p $ \style ->
    forMOf_ optic xy $ \(x,y) -> do
      liftedDrawPoint style $ Point x y






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
        True  -> foldl' (\a (Point x y) -> stepXY a x y) a0 asOutline
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
          liftedFillPath style $ [ Point x1 zero, Point x1 y, Point x2 y, Point x2 zero ]
      usingLineStype p $ \style ->
        forM_ asBars $ \(Bar x1 x2 y) ->
          liftedDrawLines style $ [ Point x1 zero, Point x1 y, Point x2 y, Point x2 zero ]
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

toOutline :: Double -> [(Double, Double)] -> [Point]
toOutline zero = concat . transformAdjacent
      ( \(x,y) -> [ Point (x-0.5) zero
                  , Point (x-0.5) y
                  , Point (x+0.5) y
                  , Point (x+0.5) zero
                  ])
      ( \(x,y)  (xB,_)        -> let dX = (xB - x)/2 in
                                   [ Point (x-dX) zero
                                   , Point (x-dX) y
                                   , Point (x+dX) y
                                   ]
      , \(xA,yA) (x,y)  (xB,_) ->
          [ Point ((xA+x)/2) yA
          , Point ((xA+x)/2) y
          , Point ((x+xB)/2) y
          ]
      , \(xA,yA) (x,y)  -> let dX = (x - xA)/2 in
                             [ Point (x-dX) yA
                             , Point (x-dX) y
                             , Point (x+dX) y
                             , Point (x+dX) zero
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

newtype Property a s = Property { prop :: Setter' s a }

instance Category Property where
  id = id
  Property f . Property g = Property (f . g)


instance IsLabel l (Property p a) => IsLabel l (Property p (Endo a)) where
  fromLabel = Property endoL . fromLabel @l


----------------------------------------
-- Plot properties

instance ( AxisValue x ~ xlim, AxisValue x ~ xlim'
         ) => IsLabel "xlim" (Property (Maybe xlim, Maybe xlim') (Plot x y)) where
  fromLabel = Property $ lens axisLimitX (\p x -> p { axisLimitX = x })

instance ( AxisValue y ~ ylim, AxisValue y ~ ylim'
         ) => IsLabel "ylim" (Property (Maybe ylim, Maybe ylim') (Plot x y)) where
  fromLabel = Property $ lens axisLimitY (\p x -> p { axisLimitY = x })

instance IsLabel "title" (Property [Char] (Plot x y)) where
  fromLabel = Property (lens plotTitle (\p x -> p { plotTitle = x }))
            . nonProp ""

instance IsLabel "title" (Property (Maybe [Char]) (Plot x y)) where
  fromLabel = Property (lens plotTitle (\p x -> p { plotTitle = x }))

instance IsLabel "grid" (Property Bool (Plot x y)) where
  fromLabel = Property (lens plotGrid (\p x -> p { plotGrid = x }))


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

----------------------------------------
-- Helpers

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
  , plotTitle   :: Maybe String
  , plotGrid    :: Bool
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
    , axisLimitX  = axisLimitX a `onFirst` axisLimitX b
    , axisLimitY  = axisLimitY a `onFirst` axisLimitY b
    , plotTitle   = getFirst $ First (plotTitle  a) <> First (plotTitle b)
    , plotGrid    = plotGrid a || plotGrid b
    }
    where
      onFirst :: forall a. (Maybe a, Maybe a) -> (Maybe a, Maybe a) -> (Maybe a, Maybe a)
      onFirst x y = coerce (coerce x <> coerce y :: (First a, First a))

instance Monoid (Plot x y) where
  mempty = Plot
    { plotObjects = []
    , axisLimitX  = (Nothing,Nothing)
    , axisLimitY  = (Nothing,Nothing)
    , plotTitle   = mempty
    , plotGrid    = False
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
