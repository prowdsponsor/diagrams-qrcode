module Diagrams.QRCode (draw, path) where

import Control.Arrow ((***))
import Data.Array (assocs)
import Data.Monoid (mempty)
import Diagrams.TwoD ( p2, square, stroke,  toTrail
                     , Path(..), QDiagram, R2, Trail )
import Codec.Binary.QRCode (toArray, Matrix, Module(..))


-- | Draw a QR code 'Matrix' into a 'QDiagram'.
draw :: Matrix -> QDiagram b R2 m
draw = stroke . path


-- | Convert a QR code 'Matrix' into a 'Path'.
path :: Matrix -> Path R2
path = Path                  -- convert to Path R2
     . fmap (p2 *** toTrail) -- convert to [(P2, Trail R2)]
     . assocs . toArray      -- convert to [((Int, Int), Module)]


-- | Convert a 'Module' to a 'Trail'.
toTrail :: Module -> Trail R2
toTrail Light = mempty
toTrail Dark  = square 1