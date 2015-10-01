{-# LANGUAGE FlexibleContexts #-}
module Diagrams.QRCode (pathList, pathMatrix, pathArray, stroke) where

import Control.Arrow ((***))
import Data.Array (assocs, Array, Ix)
import Data.Colour.Names (black, white)
import Data.Monoid (Any, mempty)
import qualified Diagrams.Attributes as D
import qualified Diagrams.Core as D
import qualified Diagrams.Located as D
import qualified Diagrams.Path as D
import qualified Diagrams.Trail as D
import qualified Diagrams.TwoD as D


-- | Stroke using default QR code colors (black on white) and
-- with the \"quiet\" region.
stroke :: (D.Backend b D.V2 Double, D.Renderable (D.Path D.V2 Double) b)
          => D.Path D.V2 Double
          -> D.QDiagram b D.V2 Double Any
stroke = D.bg white . quiet . D.fc black . D.lw D.none . D.stroke
  where
    zoneX = D.strutX 4
    zoneY = D.strutY 4
    quiet d =
                  zoneY
                  D.===
       (zoneX D.||| d D.||| zoneX)
                  D.===
                  zoneY


-- | Convert a QR code represented as a list of bounded values
-- into a 'Path'.  'minBound' values are considered to be
-- \"off\", while every other value is considered to be \"on\".
pathList :: (Bounded a, Eq a, Integral ix) => [((ix, ix), a)] -> D.Path D.V2 Double
pathList = D.Path . fmap (uncurry (flip D.at) . (p2int *** toTrail))
  where p2int = D.p2 . (fromIntegral *** fromIntegral)


-- | Same as 'pathList', but from a matrix represented as a list
-- of lists.
pathMatrix :: (Bounded a, Eq a) => [[a]] -> D.Path D.V2 Double
pathMatrix matrix =
  pathList $ do
    (r, row) <- count matrix
    (c, val) <- count row
    return ((r,c), val)
  where count = zip [(0::Int)..]


-- | Same as 'pathList', but from an array.
pathArray :: (Bounded a, Eq a, Integral ix, Ix ix) => Array (ix, ix) a -> D.Path D.V2 Double
pathArray = pathList . assocs


-- | Convert a value into a 'Trail'.
toTrail :: (Bounded a, Eq a) => a -> D.Trail D.V2 Double
toTrail x = if x == minBound then mempty else D.square 1
