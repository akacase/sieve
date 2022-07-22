{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( blastIt,
    server,
    Protocol (TCP, UDP),
    Args (..),
  )
where

import Control.Applicative (Alternative (empty))
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Crypto.TripleSec (TripleSecException, decrypt, encryptIO, runTripleSecDecryptM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, parseJSON, toJSON)
import Data.Aeson.Encoding ()
import Data.Aeson.Types
  ( FromJSON (parseJSON),
    KeyValue ((.=)),
    ToJSON (toJSON),
    Value (Object),
    object,
    (.:),
  )
import Data.Bits ()
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.List (genericDrop)
import Data.List.Split as S (splitOn)
import Data.Text as T (pack, strip, unpack)
import Data.Word ()
import Foreign.C (CInt)
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import Network.BSD (HostName, defaultProtocol)
import qualified Network.HostName as H
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
import System.IO (Handle, IOMode (AppendMode), hFlush, hPutStr, hSetBuffering, openFile)
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
    port :: Int,
    protocol :: Protocol,
    hostname :: H.HostName
  }
  deriving (Show, Generic, ToJSON)

type Name = String

type Mac = String

type IPv4 = String

type IPv6 = String

type Port = String

type Hostname = String

data Storable = Storable
  { name :: Name,
    mac :: Mac,
    ipv4 :: IPv4,
    ipv6 :: IPv6
  }
  deriving (Show, Generic, FromJSON, ToJSON)

data Storage = Storage
  { networks :: [Storable],
    scannedPort :: Int
  }
  deriving (Show, Generic, ToJSON)

instance FromJSON Storage where
  parseJSON (Object v) = Storage <$> v .: "net" <*> v .: "port"
  parseJSON _ = empty

data Protocol = TCP | UDP deriving (Show, Generic, FromJSON, ToJSON)

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
  IO (Either SomeException Handler)
open hostname port socktype protocol =
  do
    let hints = defaultHints {addrSocketType = Stream, addrFamily = AF_INET}
    addrinfos <- getAddrInfo (Just defaultHints) (Just hostname) (Just port)
    let addr = Prelude.head addrinfos

    -- Stream for TCP and Datagram for UDP
    sock <- socket (addrFamily addr) socktype $ intToCInt protocol
    t <- try $ connect sock (addrAddress addr)
    case t of
      Right _ -> return $ Right $ Handler sock (addrAddress addr)
      Left e -> return $ Left e

send :: Args -> Info -> Either SomeException Handler -> IO ()
send cmd info handler =
  case handler of
    Right h -> do
      msg <- encryptIO (C.pack $ secret cmd) $ BL.toStrict $ encode info
      sendAll
        (sock h)
        msg
      Network.Socket.close (sock h)
    Left _ -> pure ()

blast :: Hostname -> Port -> Protocol -> Args -> IO ()
blast hostname port proto cmd = do
  h <- case proto of
    TCP -> open hostname port Stream 6
    UDP -> open hostname port Datagram 17
  inter <- interfaces
  host <- H.getHostName
  t <- try $ timeout (tout cmd) $ send cmd (Info inter (read port :: Int) proto host) h :: IO (Either SomeException (Maybe ()))
  -- timeout of 2 seconds, i.e. 2000000 is default
  case t of
    Right _ -> pure ()
    Left _ -> pure ()

close :: Handler -> IO ()
close handler = Network.Socket.close (sock handler)

blastIt ::
  Args ->
  Protocol ->
  IO [()]
blastIt args protocol = do
  let hostname = endpoint args
  let comb = T.unpack . T.strip . T.pack
  let pts = S.splitOn "," (ports args)
  mapM (\p -> blast hostname p protocol args) $ fmap comb pts

interfaces :: IO [NI.NetworkInterface]
interfaces =
  NI.getNetworkInterfaces

server :: Protocol -> Args -> Handle -> IO ()
server proto args handle = withSocketsDo $ do
  sock <- case proto of
    TCP -> socket AF_INET Stream 6
    UDP -> socket AF_INET Datagram 17
  bind sock (SockAddrInet (fromIntegral (serverPort args) :: PortNumber) 0)
  case proto of
    TCP -> do
      listen sock 5
      sockHandler sock args handle
    UDP ->
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
          h <- try $ hPutStr handle $ C.unpack dec ++ "\n" :: IO (Either IOError ())
          case h of
            Right _ -> do
              hFlush handle
              pure ()
            Left _ -> pure ()
          case proto of
            TCP -> Network.Socket.close sockh
            UDP -> pure ()
        Nothing -> do
          case proto of
            TCP -> Network.Socket.close sockh
            UDP -> pure ()
