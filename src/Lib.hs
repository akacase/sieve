{-# OPTIONS_GHC -Wno-orphans #-}

module Lib
  ( blastIt,
    server,
    Protocol (TCP, UDP),
    Args (..),
  )
where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Crypto.TripleSec (decrypt, encryptIO, runTripleSecDecryptM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, toJSON, Value (Object), (.:))
import Data.Aeson.Encoding ()
import Data.Aeson.Types
  ( KeyValue ((.=)),
    object, FromJSON (parseJSON),
  )
import Data.Bits ()
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Lazy qualified as BL
import Data.List.Split as S (splitOn)
import Data.Text as T (pack, strip, unpack)
import Data.Word ()
import Foreign.C (CInt)
import GHC.Generics (Generic)
import Network.BSD (HostName)
import Network.HostName qualified as H
import Network.Info (NetworkInterface (..), getNetworkInterfaces)
import Network.Socket
  ( AddrInfo (..),
    Family (AF_INET),
    PortNumber,
    SockAddr (SockAddrInet),
    Socket,
    SocketType (Datagram, Stream),
    accept,
    bind,
    close,
    connect,
    defaultHints,
    getAddrInfo,
    listen,
    socket,
    withSocketsDo,
  )
import Network.Socket.ByteString (recv, sendAll)
import System.IO (Handle, hFlush, hPutStr)
import System.Timeout (timeout)
import Control.Applicative (empty)

intToCInt :: Int -> CInt
intToCInt = fromIntegral

instance ToJSON NetworkInterface where
  toJSON i =
    object
      [ "storableName" .= show (name i),
        "storableIPv4" .= show (ipv4 i),
        "storableIPv6" .= show (ipv6 i),
        "storableMac" .= show (mac i)
      ]

data Args = Args
  { serverPort :: Int, --  "PORT"
    ports :: String, -- "PORTS"
    endpoint :: String, -- "ENDPOINT"
    secret :: String, -- "SECRET"
    filename :: String, -- "FILENAME"
    tout :: Int -- "TIMEOUT"
  }
  deriving stock (Show, Generic, Eq)

data Info = Info
  { infoNet :: [NetworkInterface],
    infoPort :: Int,
    infoProtocol :: Protocol,
    infoHostname :: H.HostName
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

type Name = String

type Port = String

type Hostname = String

type IPv4 = String

type IPv6 = String

type Mac = String

data Storable = Storable
  { storableName :: Name,
    storableMac :: Mac,
    storableIPv4 :: IPv4,
    storableIPv6 :: IPv6
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Storage = Storage
  { networks :: [Storable],
    scannedPort :: Int
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance FromJSON Storage where
  parseJSON (Object v) = Storage <$> v .: "infoNet" <*> v .: "infoPort"
  parseJSON _ = empty

data Protocol = TCP | UDP
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Handler = Handler {handlerSock :: Socket}

open ::
  -- | Remote hostname, or localhost
  HostName ->
  -- | Port number or name
  Port ->
  -- | Type of Socket to connect
  SocketType ->
  -- | Protocol identifier
  Int ->
  -- | Handler to use for logging
  IO (Either SomeException Handler)
open hostname port socktype protocol =
  do
    addrinfos <- getAddrInfo (Just defaultHints) (Just hostname) (Just port)
    let addr = head addrinfos

    -- Stream for TCP and Datagram for UDP
    sock <- socket (addrFamily addr) socktype $ intToCInt protocol
    t <- try $ connect sock (addrAddress addr)
    case t of
      Right _ -> return $ Right $ Handler sock
      Left e -> return $ Left e

send :: Args -> Info -> Either SomeException Handler -> IO ()
send cmd info handler =
  case handler of
    Right h -> do
      msg <- encryptIO (C.pack $ secret cmd) $ BL.toStrict $ encode info
      sendAll
        (handlerSock h)
        msg
      close (handlerSock h)
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

blastIt ::
  Args ->
  Protocol ->
  IO [()]
blastIt args protocol = do
  let hostname = endpoint args
  let comb = T.unpack . T.strip . T.pack
  let pts = S.splitOn "," (ports args)
  mapM (\p -> blast hostname p protocol args) $ fmap comb pts

interfaces :: IO [NetworkInterface]
interfaces =
  getNetworkInterfaces

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
  close sock

sockHandler :: Socket -> Args -> Handle -> IO ()
sockHandler sock args handle = do
  (sockh, _) <- accept sock
  _ <- forkIO $ receiveMessage sockh TCP args handle
  sockHandler sock args handle

sockCloser :: Protocol -> Socket -> IO ()
sockCloser proto sockh = do
  case proto of
    TCP -> close sockh
    UDP -> pure ()

receiveMessage :: Socket -> Protocol -> Args -> Handle -> IO ()
receiveMessage sockh proto args handle = do
  msg <- recv sockh 4096
  print $ runTripleSecDecryptM $ decrypt (C.pack $ secret args) msg
  case runTripleSecDecryptM $ decrypt (C.pack $ secret args) msg of
    Left _ -> do
      sockCloser proto sockh
    Right dec -> do
      case decode $ BL.fromStrict dec :: Maybe Storage of
        Just _ -> do
          h <- try $ hPutStr handle $ C.unpack dec ++ "\n" :: IO (Either IOError ())
          case h of
            Right _ -> do
              hFlush handle
              pure ()
            Left _ -> pure ()
          sockCloser proto sockh
        Nothing -> do
          sockCloser proto sockh
