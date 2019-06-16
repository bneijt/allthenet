module Graphics (
    LocationColor(..),
    writeImages,
    imageSink,
    indexToLocationColor,
    mergeTilesAt,
    createTileDirectory
)
where
import Check(CheckResult(..))
import Data.Algorithm.Hilbert
import Control.Arrow ((&&&))
import Control.Monad
import Codec.Picture(PixelRGBA8(..), writePng, readPng, convertRGBA8)
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import Data.List (groupBy)
import Data.Maybe
import Data.Either
import Conduit
import qualified Data.Conduit.List as CL
import System.Directory(createDirectoryIfMissing)

-- |Size of the tiles used, tiles are always considered square in pixels. Because we want to map 2^16 by 2^16 we need zoomlevel 7 which is 128x128 tiles
tileSize :: Int
tileSize = 512

defaultZoomLevel :: Int
defaultZoomLevel = 7

write :: String -> [(Point, PixelRGBA8)] -> IO ()
write name points = do
  let background = PixelRGBA8 0 0 0 0
      img = renderDrawing tileSize tileSize background $
        forM_ points $ \p ->
            withTexture (uniformTexture (snd p)) $ fill $ rectangle (fst p) 1 1
  writePng name img

-- |Data constructor combining the location and color or a point on the map, including the tile location
data LocationColor = LocationColor {
    tileX :: Int,
    tileY :: Int,
    pixelPosition :: Point,
    color :: PixelRGBA8
} deriving (Show)

itp :: Int -> [Int]
itp idx = fromJust $ indexToPoint 16 2 idx

indexToLocationColor :: Int -> CheckResult -> LocationColor
indexToLocationColor index result = LocationColor {
        tileX = quot x tileSize,
        tileY = quot y tileSize,
        pixelPosition = V2 (fromIntegral $ mod x tileSize :: Float) (fromIntegral $ mod y tileSize :: Float),
        color = checkResultColor result
    }
    where
        [x, y] = itp index

tileName :: Int -> (Int, Int) -> String
tileName zoom (x, y) = "map/tiles/" ++ show zoom ++ "/" ++ show x ++ "_" ++ show y ++ ".png"


writeLocationBatch :: [LocationColor] -> IO ()
writeLocationBatch locations = do
    createTileDirectory defaultZoomLevel
    write outputPath $ map (pixelPosition &&& color) locations
    where
        firstLocation = head locations
        outputPath = tileName defaultZoomLevel (tileX firstLocation, tileY firstLocation)

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

createTileDirectory :: Int -> IO ()
createTileDirectory zoomlevel = createDirectoryIfMissing True $ "map/tiles/" ++ show zoomlevel

mergeTilesAt :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> IO()
mergeTilesAt zoom a b c d output = do
    putStrLn $ "from zoom " ++ show zoom ++ "; tiles" ++ show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++ " -> " ++ show output
    mergeTiles (tileName zoom a) (tileName zoom b) (tileName zoom c) (tileName zoom d) (tileName (zoom - 1) output)

-- |Merge tiles a b c and d into a single tile at output
mergeTiles :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
mergeTiles a b c d output = do
    imgA <- readPng a
    imgB <- readPng b
    imgC <- readPng c
    imgD <- readPng d
    let background = PixelRGBA8 0 0 0 0
        halfTileSize = (fromIntegral tileSize :: Float)  / 2
        img = renderDrawing tileSize tileSize background $ do
            drawOrDrop (V2 0 0) imgA
            drawOrDrop (V2 halfTileSize 0) imgB
            drawOrDrop (V2 0 halfTileSize) imgC
            drawOrDrop (V2 halfTileSize halfTileSize) imgD
        drawOrDrop loc eImg = case eImg of
            Right img -> drawImageAtSize (convertRGBA8 img) 0 loc halfTileSize halfTileSize
            Left failure -> error("Failed with " ++ failure)
    writePng output img
