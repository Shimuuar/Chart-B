{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
module Graphics.Rendering.ChartB where

import Data.Default.Class
import Data.Monoid
import Data.Distributive
import Control.Arrow ((***))
import Control.Applicative
import Control.Category
import Control.Monad
import Control.Lens
import Control.Lens.Unsound (lensProduct)
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Debug.Trace
import Prelude hiding (id,(.))
import Data.Colour
import Data.Colour.Names
import GHC.OverloadedLabels (IsLabel(..))

import Graphics.Rendering.ChartB.PlotParam



save :: Renderable r -> IO ()
save = void . Cairo.renderableToFile
  (Cairo.FileOptions (800,600) Cairo.PNG)
  "q.png"



go plt = save $ fillBackground def $ Renderable
  { minsize = return (0,0)
  , render  = \(w,h) -> do
      let viewportTransform = Matrix
            { xx = 0.9*w, yx = 0
            , xy = 0    , yy = -0.9*h
            , x0 = w*0.05
            , y0 = h*0.95
            }
          -- Compute limits
          (xA,xB) = fromRange (axisX plt) (getFirst *** getFirst $ limitX plt)
          (yA,yB) = fromRange (axisY plt) (getFirst *** getFirst $ limitY plt)
          -- FIXME: handle invalid && 1-point rnges somehow

          plotTransform = Matrix
            { xx = 1/(xB-xA), yx = 0
            , xy = 0        , yy = 1/(yB-yA)
            , x0 = -xA
            , y0 = -yA
            }
      let p0 = param plt
          p = PlotParam
              { _plotMainColor = Identity $ appEndo (_plotMainColor p0) black
              , _plotMainAlpha = Identity $ appEndo (_plotMainAlpha p0) 1
              }
      let tr = plotTransform * viewportTransform
      withClipRegion (Rect (transformP tr (Point 0 0)) (transformP tr (Point 1 1)))
        $ plot plt p tr
      return (const Nothing)
  }
  where
    fromRange _          (Just a , Just b)  = (a   , b  )
    fromRange UnknownLim (Nothing, Nothing) = (0   , 1  )
    fromRange UnknownLim (Just a,  Nothing) = (a   , a+1)
    fromRange UnknownLim (Nothing, Just b)  = (b-1 , b  )
    fromRange (MinMaxLimits a b) (Nothing, Nothing) = (a - 0.05*d, b+0.05*d)
      where d = b - a
    fromRange (MinMaxLimits _ b) (Just a, Nothing) = (a, b + 0.05*d)
      where b' = max a b
            d  = b' - a
    fromRange (MinMaxLimits a _) (Nothing, Just b) = (a' - 0.05*d, b)
      where a' = min a b
            d  = b - a'
    -- fromRange UnknownLim = (0,1)
    -- fromRange (MinMaxLimits a b)
    --   | a == b    = (a-0.5, a+0.5)
    --   | otherwise = (a - 0.05*d, b+0.05*d)
    --   where
    --     d = b - a

scatterplot :: [(Double,Double)] -> PlotObj Numeric Numeric
scatterplot xy = PlotObj
  { plot = \p tr -> do
      let style = def { _point_color = withOpacity
                          (p ^. plotMainColor . _Wrapped)
                          (p ^. plotMainAlpha . _Wrapped)
                      }
      forM_ xy $ \(x,y) ->
        drawPoint style $ transformP tr $ Point x y
  , axisX  = foldMap (\(x,_) -> MinMaxLimits x x) xy
  , axisY  = foldMap (\(_,y) -> MinMaxLimits y y) xy
  , limitX = (mempty, mempty)
  , limitY = (mempty, mempty)
  , param  = mempty
  }


x2,x3 :: [(Double,Double)]
x2 = [(x,x*x)   | x <- [0, 1e-2 .. 1.3 ]]
x3 = [(x,x*x*x) | x <- [0, 1e-2 .. 1   ]]

----------------------------------------------------------------
--
----------------------------------------------------------------



newtype Property a s = Property { prop :: Setter' s a }

instance Category Property where
  id = id
  Property f . Property g = Property (f . g)

instance (a ~ Double) => IsLabel "color" (Property (Colour a) (PlotParam Endo)) where
  fromLabel = Property $ plotMainColor . endoL

instance IsLabel "opacity" (Property Double (PlotParam Endo)) where
  fromLabel = Property $ plotMainAlpha . endoL

-- instance (a ~ Double) => IsLabel "acolor" (Property (PlotParam Endo) (AlphaColour a)) where
--   fromLabel = Property $
--     lensProduct plotMainColor plotMainAlpha . undefined

instance (a ~ Double) => IsLabel "color" (Property (Colour a) (PlotObj x y)) where
  fromLabel = Property paramL . fromLabel @"color"
instance (a ~ Double) => IsLabel "opacity" (Property (Colour a) (PlotObj x y)) where
  fromLabel = Property paramL . fromLabel @"color"

instance (AxisValue x ~ xlim, AxisValue x ~ xlim'
         ) => IsLabel "xlim" (Property (Maybe xlim, Maybe xlim') (PlotObj x y)) where
  fromLabel = Property $ lens limitX (\p x -> p { limitX = x })
                       . iso (getFirst *** getFirst) (First *** First)

instance (AxisValue y ~ ylim) => IsLabel "ylim" (Property (Maybe ylim, Maybe ylim) (PlotObj x y)) where
  fromLabel = Property $ lens limitY (\p x -> p { limitY = x })
                       . iso (getFirst *** getFirst) (First *** First)

paramL :: Lens' (PlotObj x y) (PlotParam Endo)
paramL = lens param (\x p -> x { param = p })


-- | Single entity on plog
data PlotObj x y = PlotObj
  { plot   :: PlotParam Identity -> Matrix -> BackendProgram ()
  , axisX  :: Acc x
  , axisY  :: Acc y
  , limitX :: (First (AxisValue x), First (AxisValue x))
  , limitY :: (First (AxisValue y), First (AxisValue y))
  , param  :: PlotParam Endo
  }

instance (Axis x, Axis y) => Semigroup (PlotObj x y) where
  a <> b = PlotObj
    { plot = \p m -> do plot a (applyPlotParam (param a) p) m
                        plot b (applyPlotParam (param b) p) m
    , axisX  = axisX a <> axisX b
    , axisY  = axisY a <> axisY b
    , limitX = limitX a <> limitX b
    , limitY = limitY a <> limitY b
    , param  = mempty
    }


----------------------------------------------------------------
-- Axes
----------------------------------------------------------------


class Monoid (Acc a) => Axis a where
  type Acc a
  type AxisValue a

data Numeric

data NumLimits
  = UnknownLim
  | MinMaxLimits !Double !Double
  deriving (Show)

instance Semigroup NumLimits where
  UnknownLim <> x = x
  x <> UnknownLim = x
  MinMaxLimits a1 b1 <> MinMaxLimits a2 b2 = MinMaxLimits (min a1 a2) (max b1 b2)

instance Monoid NumLimits where
  mempty = UnknownLim

instance Axis Numeric where
  type Acc       Numeric = NumLimits
  type AxisValue Numeric = Double

data Time
data Categori
