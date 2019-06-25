module Main where

import Lib
import Network.Socket
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad
import Data.IP

data Configuration = Configuration {
        shouldScan :: Bool,
        shouldDraw :: Bool,
        shouldZoom :: Bool,
        shouldPrintCoords :: Bool
    }

sample :: Parser Configuration
sample = Configuration
    <$> switch
        ( long "scan"
        <> help "Wether to scan the network into scanlog.jsonl" )
    <*>  switch
        ( long "draw"
        <> help "Wether to draw the scanlog.jsonl file" )
    <*>  switch
        ( long "zoom"
        <> help "Create different zoom levels from all generated tiles in zoom level 8" )
    <*>  switch
        ( long "print-coords"
        <> help "Print coordinates of the different network segments" )

main :: IO ()
main = greet =<< execParser opts
    where
        opts = info (sample <**> helper)
            ( fullDesc <> progDesc "Draw active hosts on the network" <> header "allthenet - draw an ip sattelite image" )


greet :: Configuration -> IO ()
greet (Configuration shouldScan shouldDraw shouldZoom shouldPrintCoords) = do
    when shouldPrintCoords printCoords
    if shouldScan && shouldDraw
        then
            scanDraw allPublicAddresses
        else do
            when shouldScan $ scan "scanlog.jsonl" allPublicAddresses
            when shouldDraw $ draw "scanlog.jsonl"
    when shouldZoom  createZoomLevels
