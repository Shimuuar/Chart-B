-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Rendering.Chart.Plot
-- Copyright   :  (c) Tim Docker 2006
-- License     :  BSD-style (see chart/COPYRIGHT)
--
-- Definitions of various types of Plots we can put on a 2D Chart.
--
-- Note that template haskell is used to derive accessor functions
-- (see 'Data.Accessor') for each field of the following data types:
--
--     * 'Plot'
--
--     * 'PlotLines'
--
--     * 'PlotPoints'
--
--     * 'PlotFillBetween'
--
--     * 'PlotErrBars'
--
--     * 'PlotBars'
-- 
--     * 'PlotHidden'
--
--     * 'PlotAnnotation'
--
-- These accessors are not shown in this API documentation.  They have
-- the same name as the field, but with the trailing underscore
-- dropped. Hence for data field f_::F in type D, they have type
--
-- @
--   f :: Data.Accessor.Accessor D F
-- @
--


{-# OPTIONS_GHC -XTemplateHaskell #-}

module Graphics.Rendering.Chart.Plot(
    Plot(..),
    ToPlot(..),
    joinPlot,
    PlotPoints(..),
    PlotErrBars(..),
    PlotLines(..),
    PlotFillBetween(..),
    ErrPoint(..),
    ErrValue(..),
    PlotBars(..),
    PlotBarsStyle(..),
    PlotBarsSpacing(..),
    PlotBarsAlignment(..),
    BarsPlotValue(..),
    PlotHidden(..),
    PlotAnnotation(..),

    symErrPoint,

    defaultPlotLineStyle,
    defaultPlotPoints,
    defaultPlotErrBars,
    defaultPlotFillBetween,
    defaultPlotLines,
    defaultPlotBars,
    defaultPlotAnnotation,

    plot_lines_title,
    plot_lines_style,
    plot_lines_values,
    plot_lines_limit_values,

    hlinePlot,
    vlinePlot,

    plot_render,
    plot_legend,
    plot_all_points,

    plot_points_title,
    plot_points_style,
    plot_points_values,

    plot_fillbetween_title,
    plot_fillbetween_style,
    plot_fillbetween_values,

    plot_errbars_title,
    plot_errbars_line_style,
    plot_errbars_tick_length,
    plot_errbars_overhang,
    plot_errbars_values,

    plotBars,
    plot_bars_style,
    plot_bars_item_styles,
    plot_bars_titles,
    plot_bars_spacing,
    plot_bars_alignment,
    plot_bars_reference,
    plot_bars_values

    ) where

import qualified Graphics.Rendering.Cairo as C
import Graphics.Rendering.Chart.Types
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Axis
import Control.Monad
import Data.List
import Data.Accessor.Template
import Data.Colour
import Data.Colour.SRGB (sRGB)
import Data.Colour.Names

-- | Interface to control plotting on a 2D area.
data Plot x y = Plot {

    -- | Given the mapping between model space coordinates and device
    --   coordinates, render this plot into a chart.
    plot_render_     :: PointMapFn x y -> CRender (),

    -- | Details for how to show this plot in a legend. For each item
    --   the string is the text to show, and the function renders a
    --   graphical sample of the plot.
    plot_legend_     :: [ (String, Rect -> CRender ()) ],

    -- | All of the model space coordinates to be plotted. These are
    --   used to autoscale the axes where necessary.
    plot_all_points_ :: ([x],[y])
}

-- | A type class abstracting the conversion of a value to a Plot.
class ToPlot a where
   toPlot :: a x y -> Plot x y

-- | Join any two plots together (they will share a legend).
joinPlot :: Plot x y -> Plot x y -> Plot x y
joinPlot Plot{ plot_render_     = renderP
             , plot_legend_     = legendP
             , plot_all_points_ = (xsP,ysP) }
         Plot{ plot_render_     = renderQ
             , plot_legend_     = legendQ
             , plot_all_points_ = (xsQ,ysQ) }

       = Plot{ plot_render_     = \a-> renderP a >> renderQ a
             , plot_legend_     = legendP ++ legendQ
             , plot_all_points_ = ( xsP++xsQ, ysP++ysQ )
             }


----------------------------------------------------------------------

mapXY :: PointMapFn x y -> ((x,y) -> Point)
mapXY f (x,y) = f (LValue x, LValue y)

----------------------------------------------------------------------

-- | Value defining a series of (possibly disjointed) lines,
--   and a style in which to render them.
data PlotLines x y = PlotLines {
    plot_lines_title_        :: String,
    plot_lines_style_        :: CairoLineStyle,
    plot_lines_values_       :: [[(x,y)]],
    plot_lines_limit_values_ :: [[(Limit x, Limit y)]]
}

instance ToPlot PlotLines where
    toPlot p = Plot {
        plot_render_     = renderPlotLines p,
        plot_legend_     = [(plot_lines_title_ p, renderPlotLegendLines p)],
        plot_all_points_ = ( map fst pts ++ xs, map snd pts ++ ys )
    }
      where
        pts = concat (plot_lines_values_ p)
        xs = [ x | (LValue x,_) <- concat (plot_lines_limit_values_ p)]
        ys = [ y | (_,LValue y) <- concat (plot_lines_limit_values_ p)]

renderPlotLines :: PlotLines x y -> PointMapFn x y -> CRender ()
renderPlotLines p pmap = preserveCState $ do
    setLineStyle (plot_lines_style_ p)
    mapM_ (drawLines (mapXY pmap)) (plot_lines_values_ p)
    mapM_ (drawLines pmap) (plot_lines_limit_values_ p)
  where
    drawLines pmap []     = return ()
    drawLines pmap (p:ps) = do
	moveTo (pmap p)
	mapM_ (\p -> lineTo (pmap p)) ps
	c $ C.stroke

renderPlotLegendLines :: PlotLines x y -> Rect -> CRender ()
renderPlotLegendLines p r@(Rect p1 p2) = preserveCState $ do
    setLineStyle (plot_lines_style_ p)
    let y = (p_y p1 + p_y p2) / 2
    moveTo (Point (p_x p1) y)
    lineTo (Point (p_x p2) y)
    c $ C.stroke

defaultPlotLineStyle :: CairoLineStyle
defaultPlotLineStyle = (solidLine 1 $ opaque blue){
     line_cap_  = C.LineCapRound,
     line_join_ = C.LineJoinRound
 }

defaultPlotLines :: PlotLines x y
defaultPlotLines = PlotLines {
    plot_lines_title_        = "",
    plot_lines_style_        = defaultPlotLineStyle,
    plot_lines_values_       = [],
    plot_lines_limit_values_ = []
}

-- | Helper function to plot a single horizontal line.
hlinePlot :: String -> CairoLineStyle -> b -> Plot a b
hlinePlot t ls v = toPlot defaultPlotLines {
    plot_lines_title_        = t,
    plot_lines_style_        = ls,
    plot_lines_limit_values_ = [[(LMin, LValue v),(LMax, LValue v)]]
    }

-- | Helper function to plot a single vertical line.
vlinePlot :: String -> CairoLineStyle -> a -> Plot a b
vlinePlot t ls v = toPlot defaultPlotLines {
    plot_lines_title_        = t,
    plot_lines_style_        = ls,
    plot_lines_limit_values_ = [[(LValue v,LMin),(LValue v,LMax)]]
    }

----------------------------------------------------------------------

-- | Value defining a series of datapoints, and a style in
--   which to render them.
data PlotPoints x y = PlotPoints {
    plot_points_title_  :: String,
    plot_points_style_  :: CairoPointStyle,
    plot_points_values_ :: [(x,y)]
}


instance ToPlot PlotPoints where
    toPlot p = Plot {
        plot_render_     = renderPlotPoints p,
        plot_legend_     = [(plot_points_title_ p, renderPlotLegendPoints p)],
        plot_all_points_ = (map fst pts, map snd pts)
    }
      where
        pts = plot_points_values_ p

renderPlotPoints :: PlotPoints x y -> PointMapFn x y -> CRender ()
renderPlotPoints p pmap = preserveCState $ do
    mapM_ (drawPoint.pmap') (plot_points_values_ p)
  where
    pmap' = mapXY pmap
    (CairoPointStyle drawPoint) = (plot_points_style_ p)


renderPlotLegendPoints :: PlotPoints x y -> Rect -> CRender ()
renderPlotLegendPoints p r@(Rect p1 p2) = preserveCState $ do
    drawPoint (Point (p_x p1)              ((p_y p1 + p_y p2)/2))
    drawPoint (Point ((p_x p1 + p_x p2)/2) ((p_y p1 + p_y p2)/2))
    drawPoint (Point (p_x p2)              ((p_y p1 + p_y p2)/2))

  where
    (CairoPointStyle drawPoint) = (plot_points_style_ p)

defaultPlotPoints :: PlotPoints x y
defaultPlotPoints = PlotPoints {
    plot_points_title_  = "",
    plot_points_style_  = defaultPointStyle,
    plot_points_values_ = []
}
----------------------------------------------------------------------
-- | Value specifying a plot filling the area between two sets of Y
--   coordinates, given common X coordinates.

data PlotFillBetween x y = PlotFillBetween {
    plot_fillbetween_title_  :: String,
    plot_fillbetween_style_  :: CairoFillStyle,
    plot_fillbetween_values_ :: [ (x, (y,y))]
}


instance ToPlot PlotFillBetween where
    toPlot p = Plot {
        plot_render_     = renderPlotFillBetween p,
        plot_legend_     = [(plot_fillbetween_title_ p,renderPlotLegendFill p)],
        plot_all_points_ = plotAllPointsFillBetween p
    }

renderPlotFillBetween :: PlotFillBetween x y -> PointMapFn x y -> CRender ()
renderPlotFillBetween p pmap =
    renderPlotFillBetween' p (plot_fillbetween_values_ p) pmap

renderPlotFillBetween' p [] _     = return ()
renderPlotFillBetween' p vs pmap  = preserveCState $ do
    setFillStyle (plot_fillbetween_style_ p)
    moveTo p0
    mapM_ lineTo p1s
    mapM_ lineTo (reverse p2s)
    lineTo p0
    c $ C.fill
  where
    pmap'    = mapXY pmap
    (p0:p1s) = map pmap' [ (x,y1) | (x,(y1,y2)) <- vs ]
    p2s      = map pmap' [ (x,y2) | (x,(y1,y2)) <- vs ]

renderPlotLegendFill :: PlotFillBetween x y -> Rect -> CRender ()
renderPlotLegendFill p r = preserveCState $ do
    setFillStyle (plot_fillbetween_style_ p)
    rectPath r
    c $ C.fill

plotAllPointsFillBetween :: PlotFillBetween x y -> ([x],[y])
plotAllPointsFillBetween p = ( [ x | (x,(_,_)) <- pts ]
                             , concat [ [y1,y2] | (_,(y1,y2)) <- pts ] )
  where
    pts = plot_fillbetween_values_ p


defaultPlotFillBetween :: PlotFillBetween x y
defaultPlotFillBetween = PlotFillBetween {
    plot_fillbetween_title_  = "",
    plot_fillbetween_style_  = solidFillStyle (opaque $ sRGB 0.5 0.5 1.0),
    plot_fillbetween_values_ = []
}

----------------------------------------------------------------------

-- | Value for holding a point with associated error bounds for each axis.

data ErrValue x = ErrValue {
      ev_low  :: x,
      ev_best :: x,
      ev_high :: x
} deriving Show

data ErrPoint x y = ErrPoint {
      ep_x :: ErrValue x,
      ep_y :: ErrValue y
} deriving Show

-- | When the error is symmetric, we can simply pass in dx for the error.
symErrPoint :: (Num a, Num b) => a -> b -> a -> b -> ErrPoint a b
symErrPoint x y dx dy = ErrPoint (ErrValue (x-dx) x (x+dx))
                                 (ErrValue (y-dy) y (y+dy))

-- | Value defining a series of error intervals, and a style in
--   which to render them.
data PlotErrBars x y = PlotErrBars {
    plot_errbars_title_       :: String,
    plot_errbars_line_style_  :: CairoLineStyle,
    plot_errbars_tick_length_ :: Double,
    plot_errbars_overhang_    :: Double,
    plot_errbars_values_      :: [ErrPoint x y]
}


instance ToPlot PlotErrBars where
    toPlot p = Plot {
        plot_render_     = renderPlotErrBars p,
        plot_legend_     = [(plot_errbars_title_ p, renderPlotLegendErrBars p)],
        plot_all_points_ = ( concat [ [ev_low x,ev_high x]
                                    | ErrPoint x _ <- pts ]
                           , concat [ [ev_low y,ev_high y]
                                    | ErrPoint _ y <- pts ] )
    }
      where
        pts = plot_errbars_values_ p

renderPlotErrBars :: PlotErrBars x y -> PointMapFn x y -> CRender ()
renderPlotErrBars p pmap = preserveCState $ do
    mapM_ (drawErrBar.epmap) (plot_errbars_values_ p)
  where
    epmap (ErrPoint (ErrValue xl x xh) (ErrValue yl y yh)) =
        ErrPoint (ErrValue xl' x' xh') (ErrValue yl' y' yh')
        where (Point x' y')   = pmap' (x,y)
              (Point xl' yl') = pmap' (xl,yl)
              (Point xh' yh') = pmap' (xh,yh)
    drawErrBar = drawErrBar0 p
    pmap'      = mapXY pmap

drawErrBar0 ps (ErrPoint (ErrValue xl x xh) (ErrValue yl y yh)) = do
        let tl = plot_errbars_tick_length_ ps
        let oh = plot_errbars_overhang_ ps
        setLineStyle (plot_errbars_line_style_ ps)
        c $ C.newPath
        c $ C.moveTo (xl-oh) y
        c $ C.lineTo (xh+oh) y
        c $ C.moveTo x (yl-oh)
        c $ C.lineTo x (yh+oh)
        c $ C.moveTo xl (y-tl)
        c $ C.lineTo xl (y+tl)
        c $ C.moveTo (x-tl) yl
        c $ C.lineTo (x+tl) yl
        c $ C.moveTo xh (y-tl)
        c $ C.lineTo xh (y+tl)
        c $ C.moveTo (x-tl) yh
        c $ C.lineTo (x+tl) yh
	c $ C.stroke

renderPlotLegendErrBars :: PlotErrBars x y -> Rect -> CRender ()
renderPlotLegendErrBars p r@(Rect p1 p2) = preserveCState $ do
    drawErrBar (symErrPoint (p_x p1)              ((p_y p1 + p_y p2)/2) dx dx)
    drawErrBar (symErrPoint ((p_x p1 + p_x p2)/2) ((p_y p1 + p_y p2)/2) dx dx)
    drawErrBar (symErrPoint (p_x p2)              ((p_y p1 + p_y p2)/2) dx dx)

  where
    drawErrBar = drawErrBar0 p
    dx         = min ((p_x p2 - p_x p1)/6) ((p_y p2 - p_y p1)/2)

defaultPlotErrBars :: PlotErrBars x y
defaultPlotErrBars = PlotErrBars {
    plot_errbars_title_       = "",
    plot_errbars_line_style_  = solidLine 1 $ opaque blue,
    plot_errbars_tick_length_ = 3,
    plot_errbars_overhang_    = 0,
    plot_errbars_values_      = []
}

----------------------------------------------------------------------

class PlotValue a => BarsPlotValue a where
    barsReference :: a
    barsAdd       :: a -> a -> a

instance BarsPlotValue Double where
    barsReference = 0
    barsAdd       = (+)
instance BarsPlotValue Int where
    barsReference = 0
    barsAdd       = (+)

data PlotBarsStyle
    = BarsStacked   -- ^ Bars for a fixed x are stacked vertically
                    --   on top of each other.
    | BarsClustered -- ^ Bars for a fixed x are put horizontally
                    --   beside each other.
     deriving (Show)

data PlotBarsSpacing
    = BarsFixWidth Double -- ^ All bars have the same width in pixels.
    | BarsFixGap Double   -- ^ There is the same interval in pixels
                          --   between adjacent bars.
     deriving (Show)

-- | How bars for a given (x,[y]) are aligned with respect to screen
--   coordinate corresponding to x (deviceX).
data PlotBarsAlignment = BarsLeft      -- ^ The left edge of bars is at deviceX
                       | BarsCentered  -- ^ The right edge of bars is at deviceX
                       | BarsRight     -- ^ Bars are centered around deviceX
     deriving (Show)

-- | Value describing how to plot a set of bars.
--   Note that the input data is typed [(x,[y])], ie for each x value
--   we plot several y values. Typically the size of each [y] list would
--   be the same.
data PlotBars x y = PlotBars {
   -- | This value specifies whether each value from [y] should be
   --   shown beside or above the previous value.
   plot_bars_style_           :: PlotBarsStyle,

   -- | The style in which to draw each element of [y]. A fill style
   --   is required, and if a linestyle is given, each bar will be
   --   outlined.
   plot_bars_item_styles_     :: [ (CairoFillStyle,Maybe CairoLineStyle) ],

   -- | The title of each element of [y]. These will be shown in the legend.
   plot_bars_titles_          :: [String],

   -- | This value controls how the widths of the bars are
   --   calculated. Either the widths of the bars, or the gaps between
   --   them can be fixed.
   plot_bars_spacing_         :: PlotBarsSpacing,

   -- | This value controls how bars for a fixed x are aligned with
   --   respect to the device coordinate corresponding to x.
   plot_bars_alignment_       :: PlotBarsAlignment,

   -- | The starting level for the chart (normally 0).
   plot_bars_reference_       :: y,

   plot_bars_singleton_width_ :: Double,

   -- | The actual points to be plotted.
   plot_bars_values_          :: [ (x,[y]) ]
}

defaultPlotBars :: BarsPlotValue y => PlotBars x y
defaultPlotBars = PlotBars {
   plot_bars_style_           = BarsClustered,
   plot_bars_item_styles_     = cycle istyles,
   plot_bars_titles_          = [],
   plot_bars_spacing_         = BarsFixGap 10,
   plot_bars_alignment_       = BarsCentered,
   plot_bars_values_          = [],
   plot_bars_singleton_width_ = 20,
   plot_bars_reference_       = barsReference
   }
  where
    istyles   = map mkstyle defaultColorSeq
    mkstyle c = (solidFillStyle c, Just (solidLine 1.0 $ opaque black))

plotBars :: (BarsPlotValue y) => PlotBars x y -> Plot x y
plotBars p = Plot {
        plot_render_     = renderPlotBars p,
        plot_legend_     = zip (plot_bars_titles_ p)
                               (map renderPlotLegendBars
                                    (plot_bars_item_styles_ p)),
        plot_all_points_ = allBarPoints p
    }

renderPlotBars :: (BarsPlotValue y) =>
                  PlotBars x y -> PointMapFn x y -> CRender ()
renderPlotBars p pmap = case (plot_bars_style_ p) of
      BarsClustered -> forM_ vals clusteredBars
      BarsStacked   -> forM_ vals stackedBars
  where
    clusteredBars (x,ys) = preserveCState $ do
       forM_ (zip3 [0,1..] ys styles) $ \(i, y, (fstyle,_)) -> do
           setFillStyle fstyle
           barPath (offset i) x yref0 y
           c $ C.fill
       forM_ (zip3 [0,1..] ys styles) $ \(i, y, (_,mlstyle)) -> do
           whenJust mlstyle $ \lstyle -> do
             setLineStyle lstyle
             barPath (offset i) x yref0 y
             c $ C.stroke

    offset = case (plot_bars_alignment_ p) of
      BarsLeft     -> \i -> fromIntegral i * width
      BarsRight    -> \i -> fromIntegral (i-nys) * width
      BarsCentered -> \i -> fromIntegral (2*i-nys) * width/2

    stackedBars (x,ys) =  preserveCState $ do
       let y2s = zip (yref0:stack ys) (stack ys)
       let ofs = case (plot_bars_alignment_ p) of {
         BarsLeft     -> 0          ;
         BarsRight    -> (-width)   ;
         BarsCentered -> (-width/2)
         }
       forM_ (zip y2s styles) $ \((y0,y1), (fstyle,_)) -> do
           setFillStyle fstyle
           barPath ofs x y0 y1
           c $ C.fill
       forM_ (zip y2s styles) $ \((y0,y1), (_,mlstyle)) -> do
           whenJust mlstyle $ \lstyle -> do
               setLineStyle lstyle
               barPath ofs x y0 y1
               c $ C.stroke

    barPath xos x y0 y1 = do
      let (Point x' y') = pmap' (x,y1)
      let (Point _ y0') = pmap' (x,y0)
      rectPath (Rect (Point (x'+xos) y0') (Point (x'+xos+width) y'))

    yref0 = plot_bars_reference_ p
    vals  = plot_bars_values_ p
    width = case plot_bars_spacing_ p of
        BarsFixGap gap -> case (plot_bars_style_ p) of
            BarsClustered -> (minXInterval - gap) / fromIntegral nys
            BarsStacked -> (minXInterval - gap)
        BarsFixWidth width -> width
    styles = plot_bars_item_styles_ p

    minXInterval = let diffs = zipWith (-) (tail mxs) mxs
                   in if null diffs
                        then plot_bars_singleton_width_ p
                        else minimum diffs
      where
        xs  = fst (allBarPoints p)
        mxs = nub $ sort $ map mapX xs

    nys    = maximum [ length ys | (x,ys) <- vals ]

    pmap'  = mapXY pmap
    mapX x = p_x (pmap' (x,barsReference))

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) f = f a
whenJust _        _ = return ()

allBarPoints :: (BarsPlotValue y) => PlotBars x y -> ([x],[y])
allBarPoints p = case (plot_bars_style_ p) of
    BarsClustered -> ( [x| (x,_) <- pts], concat [ys| (_,ys) <- pts] )
    BarsStacked   -> ( [x| (x,_) <- pts], concat [stack ys | (_,ys) <- pts] )
  where
    pts = plot_bars_values_ p
    y0  = plot_bars_reference_ p

stack :: (BarsPlotValue y) => [y] -> [y]
stack ys = scanl1 barsAdd ys


renderPlotLegendBars :: (CairoFillStyle,Maybe CairoLineStyle) -> Rect
                        -> CRender ()
renderPlotLegendBars (fstyle,mlstyle) r@(Rect p1 p2) = do
    setFillStyle fstyle
    rectPath r
    c $ C.fill

----------------------------------------------------------------------

-- | Value defining some hidden x and y values. The values don't
--   get displayed, but still affect axis scaling.
data PlotHidden x y = PlotHidden {
    plot_hidden_x_values_ :: [x],
    plot_hidden_y_values_ :: [y]
}

instance ToPlot PlotHidden where
    toPlot ph = Plot {
        plot_render_     = \_ -> return (),
        plot_legend_     = [],
        plot_all_points_ = (plot_hidden_x_values_ ph, plot_hidden_y_values_ ph)
    }



----------------------------------------------------------------------

-- | Value for describing a series of text annotations
--   to be placed at arbitrary points on the graph. Annotations
--   can be rotated and styled. Rotation angle is given in degrees,
--   rotation is performend around the anchor point.

data PlotAnnotation  x y = PlotAnnotation {
      plot_annotation_hanchor_ :: HTextAnchor,
      plot_annotation_vanchor_ :: VTextAnchor,
      plot_annotation_angle_   :: Double,
      plot_annotation_style_   :: CairoFontStyle,
      plot_annotation_values_  :: [(x,y,String)]
}


instance ToPlot PlotAnnotation where
    toPlot p = Plot {
        plot_render_ = renderAnnotation p,
	plot_legend_ = [],
	plot_all_points_ = (map (\(x,_,_)->x)  vs , map (\(_,y,_)->y) vs)
    }
      where
        vs = plot_annotation_values_ p


renderAnnotation :: PlotAnnotation x y -> PointMapFn x y -> CRender ()

renderAnnotation p pMap = sequence_ . map drawOne $ values
    where hta = plot_annotation_hanchor_ p
          vta = plot_annotation_vanchor_ p
          values = plot_annotation_values_ p
          angle =  plot_annotation_angle_ p
          style =  plot_annotation_style_ p
          drawOne (x,y,s) = drawTextS hta vta angle style point s
              where point = pMap (LValue x, LValue y)

defaultPlotAnnotation = PlotAnnotation {
                          plot_annotation_hanchor_ = HTA_Centre,
                          plot_annotation_vanchor_ = VTA_Centre,
                          plot_annotation_angle_   = 0,
                          plot_annotation_style_   = defaultFontStyle,
                          plot_annotation_values_  = []
}



----------------------------------------------------------------------
-- Template haskell to derive an instance of Data.Accessor.Accessor
-- for each field.
$( deriveAccessors ''Plot )
$( deriveAccessors ''PlotLines )
$( deriveAccessors ''PlotPoints )
$( deriveAccessors ''PlotFillBetween )
$( deriveAccessors ''PlotErrBars )
$( deriveAccessors ''PlotBars )
$( deriveAccessors ''PlotHidden )
$( deriveAccessors ''PlotAnnotation )
