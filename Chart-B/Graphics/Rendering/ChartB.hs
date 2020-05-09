{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
module Graphics.Rendering.ChartB where

import Data.Default.Class
import Data.Monoid
import Data.Distributive
import Control.Applicative
import Control.Monad
import Control.Lens
import Control.Lens.Unsound (lensProduct)
import qualified Graphics.Rendering.Chart.Backend.Cairo as Cairo
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Drawing
import Graphics.Rendering.Chart.Geometry
import Debug.Trace

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
          (xA,xB) = fromRange $ axisX plt
          (yA,yB) = fromRange $ axisY plt
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
      plot plt p (plotTransform * viewportTransform) 
      return (const Nothing)
  }
  where
    fromRange UnknownLim = (0,1)
    fromRange (MinMaxLimits a b)
      | a == b    = (a-0.5, a+0.5)
      | otherwise = (a - 0.05*d, b+0.05*d)
      where
        d = b - a

scatterplot :: [(Double,Double)] -> PlotObj Numeric Numeric
scatterplot xy = PlotObj
  { plot = \p tr -> do
      let style = def { _point_color = withOpacity
                          (p ^. plotMainColor . _Wrapped)
                          (p ^. plotMainAlpha . _Wrapped) 
                      }
      forM_ xy $ \(x,y) ->
        drawPoint style $ transformP tr $ Point x y
  , axisX = foldMap (\(x,_) -> MinMaxLimits x x) xy
  , axisY = foldMap (\(_,y) -> MinMaxLimits y y) xy
  , param = mempty
  }


x2,x3 :: [(Double,Double)]
x2 = [(x,x*x)   | x <- [0, 1e-2 .. 1.3 ]]
x3 = [(x,x*x*x) | x <- [0, 1e-2 .. 1   ]]
       
----------------------------------------------------------------
--
----------------------------------------------------------------



newtype Property s a = Property (Setter' s a)

instance (a ~ Double) => IsLabel "color" (Property (PlotParam Endo) (Colour a)) where
  fromLabel = Property $ plotMainColor . endoL

instance IsLabel "Opacity" (Property (PlotParam Endo) Double) where
  fromLabel = Property $ plotMainAlpha . endoL

-- instance (a ~ Double) => IsLabel "acolor" (Property (PlotParam Endo) (AlphaColour a)) where
--   fromLabel = Property $
--     lensProduct plotMainColor plotMainAlpha . undefined



-- | Single entity on plog
data PlotObj x y = PlotObj
  { plot  :: PlotParam Identity -> Matrix -> BackendProgram ()
  , axisX :: Acc x
  , axisY :: Acc y
  , param :: PlotParam Endo
  }

instance (Axis x, Axis y) => Semigroup (PlotObj x y) where
  a <> b = PlotObj
    { plot = \p m -> plot a p m >> plot b p m
    , axisX = axisX a <> axisX b
    , axisY = axisY a <> axisY b
    , param = param a <> param b
    }


-- zipL :: forall a b s. Lens' s a -> Lens' s b -> Lens' s (a,b)
-- zipL lA lB f s = _
--   where
--     uff = curry f
--   -- = f 

----------------------------------------------------------------
-- Axes
----------------------------------------------------------------


class Monoid (Acc a) => Axis a where
  type Acc a


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
  type Acc Numeric = NumLimits

data Time
data Categori


