-- |
module Graphics.Rendering.ChartB.Class where

import Graphics.Rendering.Chart.Geometry


-- | Values that admit affine transformation
class Transformable a where
  transformL :: Matrix -> a -> a

instance Transformable Point where
  transformL = transformP

instance Transformable Rect where
  transformL m (Rect p1 p2) = Rect (transformL m p1) (transformL m p2)

instance Transformable a => Transformable [a] where
  transformL m = fmap (transformL m)
