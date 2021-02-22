{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( blastIt,
    server,
    Protocol(TCP,UDP),
  )
where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Crypto.TripleSec (TripleSecException, decrypt, encryptIO, runTripleSecDecryptM)
import Data.Aeson
import Data.Aeson.Encoding
import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.List (genericDrop)
import Data.List.Split
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
import System.IO (hSetBuffering)
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

charToWord32 :: Char -> Word32
charToWord32 = toEnum . fromEnum

password :: C.ByteString
password = C.pack "misosoup"

ports :: [String]
ports = ["80", "443", "22", "21", "2222", "53"]

endpoint :: String
endpoint = "capsulecorp.org"

data Handle = Handle
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
  -- | Handle to use for logging
  IO Handle
open hostname port socktype protocol =
  do
    let hints = defaultHints {addrSocketType = Stream, addrFamily = AF_INET}
    addrinfos <- getAddrInfo (Just defaultHints) (Just hostname) (Just port)
    let addr = Prelude.head addrinfos

    -- Stream for TCP and Datagram for UDP
    sock <- socket (addrFamily addr) socktype $ intToCInt protocol
    connect sock (addrAddress addr)
    return $ Handle sock (addrAddress addr)

send :: Info -> Handle -> IO ()
send info handler = do
  print (encode $ info)
  msg <- (encryptIO password $ BL.toStrict $ encode info)
  sendAll
    (sock handler)
    msg
  Network.Socket.close (sock handler)

blast :: String -> Int -> Protocol -> IO (Maybe ())
blast hostname port proto = do
  h <- case proto of
    TCP -> open hostname (show port) Stream 6
    UDP -> open hostname (show port) Datagram 17
  inter <- interfaces
  -- timeout of 2 seconds
  timeout 2000000 $ send (Info inter port) h

close :: Handle -> IO ()
close handler = Network.Socket.close (sock handler)

-- TODO: sequence_ $ blastIt "capsulecorp.org" "watup" [ 1 .. 4000 ]
-- use that style on main, also, allow other protocols (UDP)
blastIt ::
  -- | Remote hostname, or localhost
  String ->
  -- | Series of ports, i.e. [ 1.. 400 ] or the defined `ports` variable
  [Int] ->
  -- | Type of Protocl -- UDP | TCP
  Protocol ->
  [IO (Maybe ())]
blastIt hostname ports protocol = fmap (\p -> blast hostname p protocol) ports

interfaces :: IO [NI.NetworkInterface]
interfaces = do
  NI.getNetworkInterfaces

server :: PortNumber -> Protocol -> IO ()
server port proto = withSocketsDo $ do
  sock <- case proto of
    TCP -> socket AF_INET Stream 6
    UDP -> socket AF_INET Datagram 17
  bind sock (SockAddrInet port 0)
  case proto of 
    TCP -> do
      listen sock 5
      sockHandler sock
    UDP -> do
      forever $ receiveMessage sock UDP
  Network.Socket.close sock

sockHandler :: Socket -> IO ()
sockHandler sock = do
  (sockh, _) <- accept sock
  forkIO $ receiveMessage sockh TCP
  sockHandler sock

receiveMessage :: Socket -> Protocol -> IO ()
receiveMessage sockh proto = do
  msg <- recv sockh 4096
  case runTripleSecDecryptM $ decrypt password msg of
    Left err -> do
      case proto of
        TCP -> Network.Socket.close sockh
        UDP -> pure ()
    Right dec -> do
      case decode $ BL.fromStrict dec :: Maybe Storage of
        Just d -> do
          print d
          case proto of
            TCP -> Network.Socket.close sockh
            UDP -> pure ()
        Nothing -> do
          case proto of
            TCP -> Network.Socket.close sockh
            UDP -> pure ()

