module Graphics (
    LocationColor(..),
    writeImages,
    imageSink,
    indexToLocationColor
)
where
import Check(CheckResult(..))
import Data.Algorithm.Hilbert

import Control.Monad
import Codec.Picture(PixelRGBA8(..), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Data.List (groupBy)
import Data.Maybe
import Conduit
import qualified Data.Conduit.List as CL

write :: String -> [(Point, PixelRGBA8)] -> IO ()
write name points = do
  let background = PixelRGBA8 0 0 0 0
      img = renderDrawing 1024 1024 background $
        forM_ points $ \x ->
            withTexture (uniformTexture (snd x)) $ fill $ rectangle (fst x) 1 1
  writePng name img

data LocationColor = LocationColor {
    tileX :: Integer,
    tileY :: Integer,
    pixelPosition :: Point,
    color :: PixelRGBA8
} deriving (Show)

itp :: Integer -> [Integer]
itp idx = fromJust $ indexToPoint 16 2 idx

indexToLocationColor :: Integer -> CheckResult -> LocationColor
indexToLocationColor index result = LocationColor {
        tileX = quot x 1024,
        tileY = quot y 1024,
        pixelPosition = V2 (fromInteger $ mod x 1024 :: Float) (fromInteger $ mod y 1024 :: Float),
        color = checkResultColor result
    }
    where
        [x, y] = itp index

writeLocationBatch :: [LocationColor] -> IO ()
writeLocationBatch locations =
    write tileName $ map (\x -> (pixelPosition x, color x)) locations
    where
        firstLocation = head locations
        tileName = show (tileX firstLocation) ++ "_" ++ show (tileY firstLocation) ++ ".png"

sameTile :: LocationColor -> LocationColor -> Bool
sameTile a b = tileX a == tileX b && tileY a == tileY b

checkResultColor :: CheckResult -> PixelRGBA8
checkResultColor c = PixelRGBA8 (if hasHttp c then 255 else 0) (if hasHttps c then 255 else 0) 0 255

imageSink :: ConduitT LocationColor Void IO ()
imageSink = imageBatch .| imagesBatchSink

imagesBatchSink :: ConduitT [LocationColor] Void IO ()
imagesBatchSink = CL.mapM_ writeLocationBatch

imageBatch :: Monad m => ConduitT LocationColor [LocationColor] m ()
imageBatch = CL.groupBy sameTile

writeImages :: [LocationColor] -> IO ()
writeImages locations = mapM_ writeLocationBatch batchedLocations
    where
        batchedLocations = groupBy sameTile locations :: [[LocationColor]]
