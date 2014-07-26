-- Copyright 2014 Romain Edelmann. All rights reserved.

-- | This module provides functions to plot distributions to files.
module Data.Distribution.Plot
    ( -- * Plotting
      plot
    , plotWith
      -- ** Options
    , PlotOptions (..)
      -- ** Lenses
    , plot_aggregator
    , plot_title
    , plot_labels
    , plot_colors
    , plot_displayer
    , plot_dimensions
    , plot_format
    , plot_extra_domain ) where

import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Data.Colour
import Data.Colour.Names
import Data.Default.Class
import Data.List (transpose)
import Data.Ratio
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Arrow (second)
import Control.Lens
import Control.Monad (void)

import Data.Distribution

-- | Options for plotting distributions.
data PlotOptions a = PlotOptions
    { getAggregator :: Aggregator a
      -- ^ Aggregator to apply on the values.
    , getTitle :: String
      -- ^ Title of the plot.
    , getLabels :: [String]
      -- ^ Labels of the distributions.
    , getColors :: [AlphaColour Double]
      -- ^ Colors for the distributions.
    , getDisplayer :: a -> String
      -- ^ How to display the values.
    , getDimensions :: (Int, Int)
      -- ^ Dimension of the output image.
    , getFormat :: FileFormat
      -- ^ Format of the output image.
    , getExtraDomain :: Set a
      -- ^ Values to display unconditionally.
    }

instance Show a => Default (PlotOptions a) where
    def = PlotOptions
        { getAggregator = id
        , getTitle = ""
        , getLabels = []
        , getColors = defaultColorSeq
        , getDisplayer = show
        , getDimensions = (600, 400)
        , getFormat = PNG
        , getExtraDomain = Set.empty }


-- Lenses

-- | Lens for 'getAggregator'.
plot_aggregator :: Simple Lens (PlotOptions a) (Aggregator a)
plot_aggregator = lens getAggregator (\ o x -> o { getAggregator = x})

-- | Lens for 'getTitle'.
plot_title :: Simple Lens (PlotOptions a) String
plot_title = lens getTitle (\ o x -> o { getTitle = x})

-- | Lens for 'getLabels'.
plot_labels :: Simple Lens (PlotOptions a) [String]
plot_labels = lens getLabels (\ o x -> o { getLabels = x})

-- | Lens for 'getColors'.
plot_colors :: Simple Lens (PlotOptions a) [AlphaColour Double]
plot_colors = lens getColors (\ o x -> o { getColors = x})

-- | Lens for 'getDisplayer'.
plot_displayer :: Simple Lens (PlotOptions a) (a -> String)
plot_displayer = lens getDisplayer (\ o x -> o { getDisplayer = x})

-- | Lens for 'getDisplayer'.
plot_dimensions :: Simple Lens (PlotOptions a) (Int, Int)
plot_dimensions = lens getDimensions (\ o x -> o { getDimensions = x})

-- | Lens for 'getFormat'.
plot_format :: Simple Lens (PlotOptions a) FileFormat
plot_format = lens getFormat (\ o x -> o { getFormat = x})

-- | Lens for 'getExtraDomain'.
plot_extra_domain :: Simple Lens (PlotOptions a) (Set a)
plot_extra_domain = lens getExtraDomain (\ o x -> o { getExtraDomain = x})


-- Plotting


-- | Plots the given distributions to PNG file.
--
--   See 'plotWith' for more options.
plot :: (Show a, Ord a) => FilePath -> [Distribution a] -> IO ()
plot = plotWith def


-- | Plots the given distributions to a file.
plotWith :: Ord a => PlotOptions a -> FilePath -> [Distribution a] -> IO ()
plotWith options file distributions = void $ renderableToFile env chart file
  where
    env = fo_format .~ getFormat options
        $ fo_size .~ getDimensions options
        $ def

    chart = toRenderable layout

    xvalues = Set.toAscList
            $ Set.unions
            $ (:) (getExtraDomain options)
            $ map support distributions

    yvalues = transpose
            $ map (map snd . getAggregator options . zip xvalues)
            $ map (\ d -> map (`probabilityAt` d) xvalues) distributions

    layout = layout_title .~ getTitle options
           $ layout_title_style . font_size .~ 10
           $ layout_y_axis . laxis_override .~
                (axis_labels %~ map (map (second (++ "%"))))
           $ layout_x_axis . laxis_generate .~ autoIndexAxis
                (map (getDisplayer options) xvalues)
           $ layout_left_axis_visibility . axis_show_ticks .~ False
           $ layout_plots .~ [ plotBars bars ]
           $ def :: Layout PlotIndex Double

    bars = plot_bars_values .~ addIndexes
            (map (map (fromRational . (* 100) . toRational)) yvalues)
         $ plot_bars_style .~ BarsClustered
         $ plot_bars_spacing .~ BarsFixGap 30 30
         $ plot_bars_item_styles .~ map makeStyle (cycle $ getColors options)
         $ plot_bars_titles .~ getLabels options
         $ def

    makeStyle c = (solidFillStyle c, Nothing)
