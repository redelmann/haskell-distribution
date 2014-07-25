
module Data.Distribution.Plot where

import Diagrams.Backend.SVG
import Diagrams.Prelude
import Diagrams.TwoD.Size

import Data.Distribution

plot :: Show a => FilePath -> Distribution a -> IO ()
plot file dist = renderSVG file (Height 600) $ toGraph $ toList dist

plotCumulative :: Show a => FilePath -> Distribution a -> IO ()
plotCumulative file dist = renderSVG file (Height 600) $ toGraph $ cumulative dist


toGraph xs = hcat' (with & sep .~ 10) ls # alignB # frame 20
  where
    ls = map line xs

    line (x, p) = vcat' (with & sep .~ 10)
        [ lineBox p
        , text (show x) # fontSize (Local 10) ]
        # alignB

    lineBox p = rect 20 (fromRational $ 500 * p)
                # fc black
                # lw veryThin
