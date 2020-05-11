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
-- |
module Graphics.Rendering.ChartB where

import Data.Default.Class
import Data.Monoid
import Data.Coerce
import Data.Foldable
import Data.Maybe
import Data.Proxy
import Control.Category
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Lens
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Prelude hiding (id,(.))
import Data.Colour
import Data.Colour.Names
import GHC.OverloadedLabels (IsLabel(..))

import Graphics.Rendering.ChartB.PlotParam
import Graphics.Rendering.ChartB.Class


save :: Renderable r -> IO ()
save = void . Cairo.renderableToFile
  (Cairo.FileOptions (800,600) Cairo.PNG)
  "q.png"



makePlot :: PlotObj Numeric Numeric -> IO ()
makePlot plt = save $ fillBackground def $ Renderable
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
      let pointFold = filterAxisX (filterAxisValues (Proxy @Numeric) (limitX plt))
                    $ filterAxisY (filterAxisValues (Proxy @Numeric) (limitY plt))
                    $ pointData plt
          -- Compute ranges for points that can appear in selected range:
          rngX = foldOverAxes pointFold (\m x _ -> m <> numLim x)
                                        (\m x   -> m <> numLim x)
                                        (\m _   -> m)
                                        mempty
          rngY = foldOverAxes pointFold (\m _ y -> m <> numLim y)
                                        (\m _   -> m)
                                        (\m y   -> m <> numLim y)
                                        mempty
          -- Merge range estimates
          (xA,xB) = fromRange rngX (limitX plt)
          (yA,yB) = fromRange rngY (limitY plt)
          plotTransform = Matrix
            { xx = 1/(xB-xA), yx = 0
            , xy = 0        , yy = 1/(yB-yA)
            , x0 = -xA / (xB - xA)
            , y0 = -yA / (yB - yA)
            }
      let tr = plotTransform * viewportTransform
      withClipRegion (transformL viewportTransform $ Rect (Point 0 0) (Point 1 1))
        $ runDrawing tr $ plot plt (param plt)
      alignStrokePoints [ transformL viewportTransform p
                        | p <- [Point 0 0, Point 0 1, Point 1 1,Point 1 0, Point 0 0]
                        ] >>= strokePointPath
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



applyFoldMap :: Monoid m => (a -> m) -> ((m -> a -> m) -> m -> m) -> m
applyFoldMap toM fld = fld (\m a -> m <> toM a) mempty



scatterplot :: [(Double,Double)] -> PlotObj Numeric Numeric
scatterplot xy = PlotObj
  { plot = \pEndo -> do
      advanceColorWheel
      p <- applyEndo pEndo <$> getDefaultPlotParam
      -- Compute style of markers
      let mPstyle = do
            s <- p ^. plotMarker . markerStyle . _Wrapped
            Just PointStyle
              { _point_color        = fromMaybe (p ^. plotMainColor . _Wrapped)
                                    $ p ^. plotMarker . markerColor . _Wrapped
              , _point_border_color = p ^. plotMarker . markerBorderColor . _Wrapped
              , _point_border_width = p ^. plotMarker . markerBorderWidth . _Wrapped
              , _point_radius       = p ^. plotMarker . markerRadius . _Wrapped
              , _point_shape        = s
              }
      forM_ mPstyle $ \style ->
        forM_ xy $ \(x,y) ->
          liftedDrawPoint style $ Point x y
      -- Compute style of lines
      let mLstyle = do
            s <- p ^. plotLines . lineDashes . _Wrapped
            Just LineStyle
              { _line_color  = fromMaybe (p ^. plotMainColor . _Wrapped)
                             $ p ^. plotLines . lineColor  . _Wrapped
              , _line_width  = p ^. plotLines . lineWidth  . _Wrapped
              , _line_dashes = s
              , _line_cap    = p ^. plotLines . lineCap    . _Wrapped
              , _line_join   = p ^. plotLines . lineJoin   . _Wrapped
              }
      forM_ mLstyle $ \style -> do
        liftedDrawLines style $ uncurry Point <$> xy
  , pointData  = FoldOverAxes $ \stepXY _ _ a0 -> foldl' (\a (x,y) -> stepXY a x y) a0 xy
  , limitX = (Nothing, Nothing)
  , limitY = (Nothing, Nothing)
  , param  = mempty
  }


x2,x3 :: [(Double,Double)]
x2 = [(x,x*x)   | x <- [0, 1e-2 .. 1.3 ]]
x3 = [(x,x*x*x) | x <- [0, 1e-2 .. 1   ]]


go = mconcat
  [ scatterplot x2
    & prop #color .~ opaque blue
    & prop (#marker . #size) .~ (4 :: Double)
  , scatterplot x3 & prop #color .~ opaque red
  ]
  & prop #xlim .~ (Nothing, Just 1)

go2 = mconcat
  [ scatterplot x2
  , scatterplot x3
  ]


----------------------------------------------------------------
--
----------------------------------------------------------------



newtype Property a s = Property { prop :: Setter' s a }

instance Category Property where
  id = id
  Property f . Property g = Property (f . g)

instance (a ~ Double) => IsLabel "color" (Property (AlphaColour a) (PlotParam Endo)) where
  fromLabel = Property $ plotMainColor . endoL

-- instance IsLabel "opacity" (Property Double (PlotParam Endo)) where
--   fromLabel = Property $ plotMainAlpha . endoL

-- instance (a ~ Double) => IsLabel "acolor" (Property (PlotParam Endo) (AlphaColour a)) where
--   fromLabel = Property $
--     lensProduct plotMainColor plotMainAlpha . undefined

instance (a ~ Double) => IsLabel "color" (Property (AlphaColour a) (PlotObj x y)) where
  fromLabel = Property paramL . fromLabel @"color"
-- instance (a ~ Double) => IsLabel "opacity" (Property (Colour a) (PlotObj x y)) where
--   fromLabel = Property paramL . fromLabel @"color"

instance (p ~ MarkerParam Endo) => IsLabel "marker" (Property p (PlotObj x y)) where
  fromLabel = Property (paramL . plotMarker)

instance ( AxisValue x ~ xlim, AxisValue x ~ xlim'
         ) => IsLabel "xlim" (Property (Maybe xlim, Maybe xlim') (PlotObj x y)) where
  fromLabel = Property $ lens limitX (\p x -> p { limitX = x })

instance ( AxisValue y ~ ylim, AxisValue y ~ ylim'
         ) => IsLabel "ylim" (Property (Maybe ylim, Maybe ylim') (PlotObj x y)) where
  fromLabel = Property $ lens limitY (\p x -> p { limitY = x })

instance IsLabel "shape" (Property (Maybe PointShape) (MarkerParam Endo)) where
  fromLabel = Property $ markerStyle . endoL

instance IsLabel "shape" (Property PointShape (MarkerParam Endo)) where
  fromLabel = Property $ markerStyle . endoL . fun
    where
      fun f Nothing  = Just <$> f PointShapeCircle
      fun f (Just x) = Just <$> f x

instance IsLabel "size" (Property Double (MarkerParam Endo)) where
  fromLabel = Property $ markerRadius . endoL



paramL :: Lens' (PlotObj x y) (PlotParam Endo)
paramL = lens param (\x p -> x { param = p })


-- | Single entity on plog
data PlotObj x y = PlotObj
  { plot      :: PlotParam Endo -> Drawing ()
  , pointData :: FoldOverAxes x y
  , limitX    :: (Maybe (AxisValue x), Maybe (AxisValue x))
  , limitY    :: (Maybe (AxisValue y), Maybe (AxisValue y))
  , param     :: PlotParam Endo
  }

instance (Axis x, Axis y) => Semigroup (PlotObj x y) where
  a <> b = PlotObj
    { plot = \p -> do plot a (p <> param a)
                      plot b (p <> param b)
    , pointData = pointData a <> pointData b
    , limitX = limitX a `onFirst` limitX b
    , limitY = limitY a `onFirst` limitY b
    , param  = mempty
    }
    where
      onFirst :: forall a. (Maybe a, Maybe a) -> (Maybe a, Maybe a) -> (Maybe a, Maybe a)
      onFirst x y = coerce (coerce x <> coerce y :: (First a, First a))

instance (Axis x, Axis y) => Monoid (PlotObj x y) where
  mempty = PlotObj
    { plot      = \_ -> return ()
    , pointData = FoldOverAxes $ \_ _ _ -> id
    , limitX    = (Nothing,Nothing)
    , limitY    = (Nothing,Nothing)
    , param     = mempty
    }


----------------------------------------------------------------
-- Axes
----------------------------------------------------------------

-- | Fold which is used to compute maximum and minimum value for
--   axis autorange
newtype FoldOverAxes x y = FoldOverAxes
  { foldOverAxes :: forall a. (a -> AxisValue x -> AxisValue y -> a)
                           -> (a -> AxisValue x -> a)
                           -> (a -> AxisValue y -> a)
                           -> (a -> a)
  }

instance Semigroup (FoldOverAxes x y) where
  FoldOverAxes f <> FoldOverAxes g = FoldOverAxes $ \stepXY stepX stepY ->
    g stepXY stepX stepY . f stepXY stepX stepY

instance Monoid (FoldOverAxes x y) where
  mempty = FoldOverAxes $ \_ _ _ -> id

filterAxisX :: (AxisValue x -> Bool) -> FoldOverAxes x y -> FoldOverAxes x y
filterAxisX predX (FoldOverAxes fun) = FoldOverAxes $ \stepXY stepX
  -> fun (\a x y -> if predX x then stepXY a x y else a)
         (\a x   -> if predX x then stepX  a x   else a)

filterAxisY :: (AxisValue y -> Bool) -> FoldOverAxes x y -> FoldOverAxes x y
filterAxisY predY (FoldOverAxes fun) = FoldOverAxes $ \stepXY stepX stepY
  -> fun (\a x y -> if predY y then stepXY a x y else a)
         stepX
         (\a y   -> if predY y then stepY  a y   else a)



----------------------------------------------------------------
-- Axes
----------------------------------------------------------------


class Axis a where
  type AxisValue a
  filterAxisValues :: Proxy a -> (Maybe (AxisValue a), Maybe (AxisValue a)) -> AxisValue a -> Bool


data Numeric



data NumLimits
  = UnknownLim
  | MinMaxLimits !Double !Double
  deriving (Show)

instance Semigroup NumLimits where
  UnknownLim <> x = x
  x <> UnknownLim = x
  MinMaxLimits a1 b1 <> MinMaxLimits a2 b2 = MinMaxLimits (min a1 a2) (max b1 b2)

numLim :: Double -> NumLimits
numLim x = MinMaxLimits x x

instance Monoid NumLimits where
  mempty = UnknownLim

instance Axis Numeric where
  type AxisValue Numeric = Double
  filterAxisValues _ (Nothing, Nothing) _ = True
  filterAxisValues _ (Just a,  Nothing) x = x >= a
  filterAxisValues _ (Nothing, Just b ) x = x <= b
  filterAxisValues _ (Just a,  Just b ) x = x >= a && x <= b


data Time
data Categori




----------------------------------------------------------------
-- Drawing monad
----------------------------------------------------------------

newtype Drawing a = Drawing (StateT [AlphaColour Double] (ReaderT Matrix BackendProgram) a)
  deriving newtype (Functor, Applicative, Monad)

runDrawing :: Matrix -> Drawing a -> BackendProgram a
runDrawing tr (Drawing act)
  = flip runReaderT tr
  $ evalStateT act defColors
  where
    defColors = opaque black
              : cycle (map opaque $ [blue, red, green, yellow, cyan, magenta])


advanceColorWheel :: Drawing ()
advanceColorWheel = Drawing $ modify tail

getDefaultPlotParam :: Drawing (PlotParam Identity)
getDefaultPlotParam = Drawing $ do
  c <- head <$> get
  return $ def & plotMainColor . _Wrapped .~ c

liftedDrawPoint :: PointStyle -> Point -> Drawing ()
liftedDrawPoint st p = do
  tr <- Drawing ask
  Drawing $ lift $ lift $ drawPoint st (transformL tr p)


liftedDrawLines :: LineStyle -> [Point] -> Drawing ()
liftedDrawLines style pts = Drawing $ do
  tr <- ask
  lift $ lift $ withLineStyle style $ alignStrokePoints (transformL tr <$> pts) >>= strokePointPath
