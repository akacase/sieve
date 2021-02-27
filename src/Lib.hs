{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( blastIt,
    server,
    Protocol (TCP, UDP),
    Args (..),
  )
where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Crypto.TripleSec (TripleSecException, decrypt, encryptIO, runTripleSecDecryptM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, parseJSON, toJSON)
import Data.Aeson.Encoding
import Data.Aeson.Types
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.List (genericDrop)
import Data.List.Split as S
import Data.Text as T
import Data.Word
import Foreign.C
import Foreign.C.Types (CInt)
import GHC.Generics
import Network.BSD (HostName, defaultProtocol)
import qualified Network.Info as NI
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily, addrProtocol, addrSocketType),
    Family (AF_INET),
    HostName,
    PortNumber,
    SockAddr (SockAddrInet),
    Socket,
    SocketOption (SendTimeOut, UserTimeout),
    SocketType (Datagram, Raw, Stream),
    accept,
    bind,
    close,
    connect,
    defaultHints,
    defaultProtocol,
    getAddrInfo,
    listen,
    setSocketOption,
    socket,
    withSocketsDo,
  )
import Network.Socket.Address (sendAllTo, sendTo)
import Network.Socket.ByteString (recv, sendAll)
import System.IO (Handle, IOMode (ReadWriteMode), hPutStr, hSetBuffering, openFile)
import System.Timeout (timeout)

intToCInt :: Int -> CInt
intToCInt = fromIntegral

instance ToJSON NI.NetworkInterface where
  toJSON i =
    object
      [ "name" .= show (NI.name i),
        "ipv4" .= show (NI.ipv4 i),
        "ipv6" .= show (NI.ipv6 i),
        "mac" .= show (NI.mac i)
      ]

data Args = Args
  { serverPort :: Int, --  "PORT"
    ports :: String, -- "PORTS"
    endpoint :: String, -- "ENDPOINT"
    secret :: String, -- "SECRET"
    filename :: String, -- "FILENAME"
    tout :: Int -- "TIMEOUT"
  }
  deriving (Show, Generic, Eq)

data Info = Info
  { net :: [NI.NetworkInterface],
    port :: Int
  }
  deriving (Show, Generic, ToJSON)

data Storable = Storable
  { name :: String,
    mac :: String,
    ipv4 :: String,
    ipv6 :: String
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Storage = Storage
  { networks :: [Storable],
    scannedPort :: Int
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON Storage where
  parseJSON (Object v) = Storage <$> v .: "net" <*> v .: "port"

data Protocol = TCP | UDP

data Handler = Handler
  { sock :: Socket,
    address :: SockAddr
  }

open ::
  -- | Remote hostname, or localhost
  HostName ->
  -- | Port number or name
  String ->
  -- | Type of Socket to connect
  SocketType ->
  -- | Protocol identifier
  Int ->
  -- | Handler to use for logging
  IO Handler
open hostname port socktype protocol =
  do
    let hints = defaultHints {addrSocketType = Stream, addrFamily = AF_INET}
    addrinfos <- getAddrInfo (Just defaultHints) (Just hostname) (Just port)
    let addr = Prelude.head addrinfos

    -- Stream for TCP and Datagram for UDP
    sock <- socket (addrFamily addr) socktype $ intToCInt protocol
    connect sock (addrAddress addr)
    return $ Handler sock (addrAddress addr)

send :: Args -> Info -> Handler -> IO ()
send cmd info handler = do
  msg <- (encryptIO (C.pack $ secret cmd) $ BL.toStrict $ encode info)
  sendAll
    (sock handler)
    msg
  Network.Socket.close (sock handler)

blast :: String -> String -> Protocol -> Args -> IO ()
blast hostname port proto cmd = do
  h <- case proto of
    TCP -> open hostname port Stream 6
    UDP -> open hostname port Datagram 17
  inter <- interfaces
  t <- timeout (tout cmd) $ send cmd (Info inter (read port :: Int)) h
  -- timeout of 2 seconds, i.e. 2000000 is default
  case t of
    Just _ -> pure ()
    Nothing -> pure ()

close :: Handler -> IO ()
close handler = Network.Socket.close (sock handler)

blastIt ::
  Args ->
  Protocol ->
  IO ()
blastIt args protocol = do
  let hostname = (endpoint args)
  let pts = S.splitOn "," (ports args)
  -- place holder to hold error values
  let t = fmap (\p -> blast hostname p protocol args) pts
  pure ()

interfaces :: IO [NI.NetworkInterface]
interfaces = do
  NI.getNetworkInterfaces

server :: Protocol -> Args -> IO ()
server proto args = withSocketsDo $ do
  sock <- case proto of
    TCP -> socket AF_INET Stream 6
    UDP -> socket AF_INET Datagram 17
  bind sock (SockAddrInet (fromIntegral (serverPort args) :: PortNumber) 0)
  case proto of
    TCP -> do
      let file = filename args ++ "_tcp.json"
      handle <- openFile file  ReadWriteMode
      listen sock 5
      sockHandler sock args handle
    UDP -> do
      let file = filename args ++ "_udp.json"
      handle <- openFile file ReadWriteMode
      forever $ receiveMessage sock UDP args handle
  Network.Socket.close sock

sockHandler :: Socket -> Args -> Handle -> IO ()
sockHandler sock args handle = do
  (sockh, _) <- accept sock
  forkIO $ receiveMessage sockh TCP args handle
  sockHandler sock args handle

receiveMessage :: Socket -> Protocol -> Args -> Handle -> IO ()
receiveMessage sockh proto args handle = do
  msg <- recv sockh 4096
  case runTripleSecDecryptM $ decrypt (C.pack $ secret args) msg of
    Left err -> do
      case proto of
        TCP -> Network.Socket.close sockh
        UDP -> pure ()
    Right dec -> do
      case decode $ BL.fromStrict dec :: Maybe Storage of
        Just d -> do
          putStrLn $ show d
          hPutStr handle $ C.unpack dec
          case proto of
            TCP -> Network.Socket.close sockh
            UDP -> pure ()
        Nothing -> do
          case proto of
            TCP -> Network.Socket.close sockh
            UDP -> pure ()
