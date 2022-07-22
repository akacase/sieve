module Lib
  ( blastIt,
    server,
    Protocol (TCP, UDP),
    Args (..), close')
where

import Control.Applicative (Alternative (empty))
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Crypto.TripleSec (decrypt, encryptIO, runTripleSecDecryptM)
import Data.Aeson (FromJSON, ToJSON, decode, encode, parseJSON, toJSON)
import Data.Aeson.Encoding ()
import Data.Aeson.Types
  ( KeyValue ((.=)),
    Value (Object),
    object,
    (.:),
  )
import Data.Bits ()
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Data.List.Split as S (splitOn)
import Data.Text as T (pack, strip, unpack)
import Data.Word ()
import Foreign.C (CInt)
import GHC.Generics (Generic)
import Network.BSD (HostName)
import qualified Network.HostName as H
import qualified Network.Info as NI
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
  deriving stock (Show, Generic, Eq)

data Info = Info
  { infoNet :: [NI.NetworkInterface],
    infoPort :: Int,
    infoProtocol :: Protocol,
    infoHostname :: H.HostName
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

type Name = String

type Mac = String

type IPv4 = String

type IPv6 = String

type Port = String

type Hostname = String

type Socket = String

data Storable = Storable
  { name :: Name,
    mac :: Mac,
    ipv4 :: IPv4,
    ipv6 :: IPv6
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
  parseJSON (Object v) = Storage <$> v .: "net" <*> v .: "port"
  parseJSON _ = empty

data Protocol = TCP | UDP 
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


data Handler = Handler
  { handlerSock :: Network.Socket.Socket,
    handlerAddress :: Network.Socket.SockAddr
  }

open ::
  -- | Remote hostname, or localhost
  HostName ->
  -- | Port number or name
  String ->
  -- | Type of Socket to connect
  Network.Socket.SocketType ->
  -- | Protocol identifier
  Int ->
  -- | Handler to use for logging
  IO (Either SomeException Handler)
open hostname port socktype protocol =
  do
    addrinfos <- Network.Socket.getAddrInfo (Just Network.Socket.defaultHints) (Just hostname) (Just port)
    let addr = Prelude.head addrinfos

    -- Stream for TCP and Datagram for UDP
    sock <- Network.Socket.socket (Network.Socket.addrFamily addr) socktype $ intToCInt protocol
    t <- try $ Network.Socket.connect sock (Network.Socket.addrAddress addr)
    case t of
      Right _ -> return $ Right $ Handler sock (Network.Socket.addrAddress addr)
      Left e -> return $ Left e

send :: Args -> Info -> Either SomeException Handler -> IO ()
send cmd info handler =
  case handler of
    Right h -> do
      msg <- encryptIO (C.pack $ secret cmd) $ BL.toStrict $ encode info
      sendAll
        (handlerSock h)
        msg
      Network.Socket.close (handlerSock h)
    Left _ -> pure ()

blast :: Hostname -> Port -> Protocol -> Args -> IO ()
blast hostname port proto cmd = do
  h <- case proto of
    TCP -> open hostname port Network.Socket.Stream 6
    UDP -> open hostname port Network.Socket.Datagram 17
  inter <- interfaces
  host <- H.getHostName
  t <- try $ timeout (tout cmd) $ send cmd (Info inter (read port :: Int) proto host) h :: IO (Either SomeException (Maybe ()))
  -- timeout of 2 seconds, i.e. 2000000 is default
  case t of
    Right _ -> pure ()
    Left _ -> pure ()

close' :: Handler -> IO ()
close' handler = Network.Socket.close (handlerSock handler)

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
server proto args handle = Network.Socket.withSocketsDo $ do
  sock <- case proto of
    TCP -> Network.Socket.socket Network.Socket.AF_INET Network.Socket.Stream 6
    UDP -> Network.Socket.socket Network.Socket.AF_INET Network.Socket.Datagram 17
  Network.Socket.bind sock (Network.Socket.SockAddrInet (fromIntegral (serverPort args) :: Network.Socket.PortNumber) 0)
  case proto of
    TCP -> do
      Network.Socket.listen sock 5
      sockHandler sock args handle
    UDP ->
      forever $ receiveMessage sock UDP args handle
  Network.Socket.close sock

sockHandler :: Network.Socket.Socket -> Args -> Handle -> IO ()
sockHandler sock args handle = do
  (sockh, _) <- Network.Socket.accept sock
  _ <- forkIO $ receiveMessage sockh TCP args handle
  sockHandler sock args handle

receiveMessage :: Network.Socket.Socket -> Protocol -> Args -> Handle -> IO ()
receiveMessage sockh proto args handle = do
  msg <- recv sockh 4096
  case runTripleSecDecryptM $ decrypt (C.pack $ secret args) msg of
    Left _ -> do
      case proto of
        TCP -> Network.Socket.close sockh
        UDP -> pure ()
    Right dec -> do
      case decode $ BL.fromStrict dec :: Maybe Storage of
        Just _ -> do
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
