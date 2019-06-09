{-# LANGUAGE OverloadedStrings #-}
{-|
Two main module paths: run through host address list and output to logFile
-}
module Lib
    (
    scan,
    draw,
    scanDraw,
    testNet,
    allPublicAddresses,
    createZoomLevels
    ) where
import Data.Algorithm.Hilbert
import Data.Maybe
import Graphics.Rasterific (Point, V2(..))
import Codec.Picture(PixelRGBA8(..))
import Check
import Data.IP
import Graphics
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Aeson
import System.IO
import Conduit
import qualified Data.Conduit.List as CL
import Control.Concurrent.Async (mapConcurrently)
import Data.List.Split (chunksOf)

testNet = [toIPv4 [192,168,a, b] |
    a <- [0..255],
    b <- [0..255]
    ]

allAddresses :: [IPv4]
allAddresses = [toIPv4 [a, b, c, d] |
    a <- [0..255],
    b <- [0..255],
    c <- [0..255],
    d <- [0..255]
    ]

blacklist :: [AddrRange IPv4]
blacklist = map (\x -> read x :: AddrRange IPv4) [
    "0.0.0.0/8", --            "This" Network                 [RFC1700, page 4]
    "10.0.0.0/8", --          Private-Use Networks                   [RFC1918]
    "14.0.0.0/8",   --        Public-Data Networks         [RFC1700, page 181]
    "24.0.0.0/8", --           Cable Television Networks                    --
    "39.0.0.0/8", --           Reserved but subject                       to allocation                       [RFC1797]
    "127.0.0.0/8", --          Loopback                       [RFC1700, page 5]
    "128.0.0.0/16", --         Reserved but subject to allocation                             --
    "169.254.0.0/16", --       Link Local                                   --
    "172.16.0.0/12", --        Private-Use Networks                   [RFC1918]
    "191.255.0.0/16", --       Reserved but subject to allocation                             --
    "192.0.0.0/24", --         Reserved but subject to allocation                             --
    "192.0.2.0/24", --         Test-Net
    "192.88.99.0/24", --       6to4 Relay Anycast                     [RFC3068]
    "192.168.0.0/16", --       Private-Use Networks                   [RFC1918]
    "198.18.0.0/15", --        Network Interconnect
    "223.255.255.0/24", --     Reserved but subject  to allocation                             --
    "224.0.0.0/4", ---          Multicast                              [RFC3171]
    "240.0.0.0/4" --          Reserved for Future Use        [RFC1700, page 4]
    ]

isBlacklisted :: IPv4 -> Bool
isBlacklisted addr = any (isMatchedTo addr) blacklist

allPublicAddresses :: [IPv4]
allPublicAddresses = filter (not . isBlacklisted) allAddresses

scanDraw :: [IPv4] -> IO()
scanDraw addresses = runConduit $ yieldMany addresses .| runCheck .| checkResultLocation .| imageSink

-- |Scan the given addresses and append check results to the given path
scan :: FilePath -> [IPv4] -> IO ()
scan logPath addresses =
    withFile logPath AppendMode $ \fileHandle ->
        runConduit $ yieldMany addresses .| runCheck .| encodeCheck .| sinkHandle fileHandle

-- |Scan the given host and append the result to the given path
runCheck :: ConduitT IPv4 CheckResult IO () -- converts Ints into Strings
runCheck = mapAsync 5000 check

encodeCheck :: ConduitT CheckResult B.ByteString IO ()
encodeCheck = mapC $ \result -> B.append (L.toStrict (encode result)) "\n"

hostAddressToList :: IPv4 -> [Int]
hostAddressToList hostAddress = map fromIntegral (fromIPv4 hostAddress)

addressToIndex :: IPv4 -> Int
addressToIndex = sum . map (\(n,o) -> o * 256 ^ n) . zip [0..] . reverse . hostAddressToList

loadCheckResults :: Handle -> IO [CheckResult]
loadCheckResults handle = do
    eof <- hIsEOF handle
    if eof
        then return []
        else do
            line <- L.fromStrict <$> B.hGetLine handle
            case (decode line :: Maybe CheckResult) of
                Just result -> do
                    rest <- loadCheckResults handle
                    return $ result : rest
                Nothing -> do
                    _ <- LC.putStrLn $ L.append "Failed to parse: " line
                    loadCheckResults handle

checkResultLocation :: Monad m => ConduitT CheckResult LocationColor m ()
checkResultLocation = mapC checkResultToLocation

checkResultToLocation :: CheckResult -> LocationColor
checkResultToLocation cr = indexToLocationColor index cr
    where
        index = addressToIndex (address cr)

draw :: FilePath -> IO ()
draw logPath =
    withFile logPath ReadMode $ \handle -> do
        checkResults <- loadCheckResults handle
        writeImages $ map checkResultToLocation checkResults

-- | Map `a` asynchronously to `b` using batches of at most `n` elements
mapAsync :: Int -> (a -> IO b) -> ConduitT a b IO ()
mapAsync n op = CL.chunksOf n .| mapAsynced .| CL.concat
    where
        mapAsynced = mapMC (mapConcurrently op)

createZoomLevel :: Int -> IO()
createZoomLevel newZoom = do
    createTileDirectory newZoom
    mapM_ (\(outPos, [a,b,c,d]) -> mergeTilesAt oldZoom a b c d outPos) mapping
        where
            oldZoom = newZoom + 1
            newTiles = [(x, y) | y <- [0..2^newZoom -1], x <- [0..2^newZoom - 1]]
            oldTilesFor (oldX, oldY) = [(x, y) | y <- take 2 [2*oldY..], x <- take 2 [2*oldX..]]
            mapping = zip newTiles (map oldTilesFor newTiles)

createZoomLevels :: IO()
createZoomLevels = mapM_ createZoomLevel [6,5..0]
