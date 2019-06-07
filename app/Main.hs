module Main where

import Lib
import Network.Socket
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad
import Data.IP

data Configuration = Configuration {
        shouldScan      :: Bool,
        shouldDraw      :: Bool
    }

sample :: Parser Configuration
sample = Configuration
    <$> switch
        ( long "scan"
        <> help "Wether to scan the network into scanlog.jsonl" )
    <*>  switch
        ( long "draw"
        <> help "Wether to draw the scanlog.jsonl file" )

main :: IO ()
main = greet =<< execParser opts
    where
        opts = info (sample <**> helper)
            ( fullDesc <> progDesc "Draw activet hosts on the network" <> header "allthenet - draw a network map" )


greet :: Configuration -> IO ()
greet (Configuration shouldScan shouldDraw) =
    if shouldScan && shouldDraw
        then
            scanDraw allPublicAddresses
        else do
            when shouldScan $ scan "scanlog.jsonl" allPublicAddresses
            when shouldDraw $ draw "scanlog.jsonl"
