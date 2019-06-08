{-# LANGUAGE DeriveGeneric #-}
module Check (
    check,
    CheckResult(..)
) where

import GHC.Generics
import Network.Socket
import Control.Exception(try, bracket, SomeException)
import System.IO(Handle)
import Data.Time.LocalTime
import Data.Aeson
import System.Timeout
import Data.Maybe
import qualified Data.ByteString.Lazy as BS
import Data.IP
import Data.Aeson.IP
-- colorHost :: String -> IO PixelRGBA8
-- colorHost ip = do
--     hasHttp <- isPortOpen ip 80
--     hasHttps <- isPortOpen ip 443
--     return $ PixelRGBA8 (if hasHttp then 255 else 0) (if hasHttps then 255 else 0) 0 255
--
-- data Host = HostAddress
--     deriving (Show)

connectToHost :: IPv4 -> PortNumber -> IO Bool
connectToHost address port = withSocketsDo $ do
    addr <- resolve host (show port)
    bracket (openSocket addr) closeSocket tryConnectingSocket
    where
        host = show address
        resolve host port = do
            let hints = defaultHints { addrSocketType = Stream }
            addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
            return addr
        closeSocket (sock, _) = close sock
        tryConnectingSocket (sock, addr) = do
            connectAttempt <- timeout 1000000 $ connect sock $ addrAddress addr
            case connectAttempt of
                Just _ -> return True
                Nothing -> return False
        openSocket addr = do
            s <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            return (s, addr)

isPortOpen :: IPv4 -> PortNumber -> IO Bool
isPortOpen addr port = do
    result <- try $ connectToHost addr port :: IO (Either SomeException Bool)
    case result of
        Left _ -> return False
        Right result -> return result

check :: IPv4 -> IO CheckResult
check addr = do
    now <- getZonedTime
    hasHttp <- isPortOpen addr 80
    hasHttps <- isPortOpen addr 443
    return CheckResult {
        address = addr,
        hasHttp = hasHttp,
        hasHttps = hasHttps,
        timeOfCheck = now
    }

showHostAddress :: HostAddress -> String
showHostAddress addr = concat [show a, ".", show b, ".", show c, ".", show d]
    where
        (a,b,c,d) = hostAddressToTuple addr

data CheckResult = CheckResult {
    address :: IPv4,
    hasHttp :: Bool,
    hasHttps :: Bool,
    timeOfCheck :: ZonedTime
} deriving (Generic, Show)

instance ToJSON CheckResult where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON CheckResult




-- instance ToJSON Host where
--     toJSON = toValue showHostAddress
