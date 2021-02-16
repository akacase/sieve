{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Lib
  ( blastIt,
  )
where

import Crypto.TripleSec (decrypt, encryptIO)
import Data.Aeson
import Data.Aeson.Encoding
import Data.Bits
import qualified Data.ByteString as B
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
    SockAddr,
    Socket,
    SocketOption (SendTimeOut, UserTimeout),
    SocketType (Raw, Stream),
    close,
    connect,
    defaultHints,
    defaultProtocol,
    getAddrInfo,
    setSocketOption,
    socket,
  )
import Network.Socket.Address (sendAllTo, sendTo)
import Network.Socket.ByteString (recv, sendAll)
import System.Timeout (timeout)

instance ToJSON NI.NetworkInterface where
  toJSON i =
    object
      [ "name" .= show (NI.name i),
        "ipv4" .= show (NI.ipv4 i),
        "ipv6" .= show (NI.ipv6 i)
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

charToWord32 :: Char -> Word32
charToWord32 = toEnum . fromEnum

password :: C.ByteString
password = C.pack "misosoup"

ports :: [String]
ports = ["80", "443", "22", "21", "2222", "53"]

endpoint :: String
endpoint = "capsulecorp.org"

secret :: String
secret = "sup"

detection :: String
detection = "yo"

data Handle = Handle
  { sock :: Socket,
    address :: SockAddr
  }

open ::
  -- | Remote hostname, or localhost
  HostName ->
  -- | Port number or name
  String ->
  -- | Handle to use for logging
  IO Handle
open hostname port =
  do
    -- Look up the hostname and port.  Either raises an exception
    -- or returns a nonempty list.  First element in that list
    -- is supposed to be the best option.
    let hints = defaultHints {addrSocketType = Stream, addrFamily = AF_INET}
    addrinfos <- getAddrInfo (Just defaultHints) (Just hostname) (Just port)
    let addr = Prelude.head addrinfos

    -- Establish a socket for communication
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    -- Save off the socket, program name, and server address in a handle
    return $ Handle sock (addrAddress addr)

-- Now you may use connectionSocket as you please within this scope,
-- possibly using recv and send to interact with the remote end.

toStrict :: BL.ByteString -> B.ByteString
toStrict = B.concat . BL.toChunks

send :: Info -> Handle -> IO ()
send info handler = do
  print (encode $ info)
  msg <- (encryptIO password $ toStrict $ encode info)
  sendAll
    (sock handler)
    msg
  Network.Socket.close (sock handler)

blast :: String -> Int -> IO (Maybe ())
blast hostname port = do
  h <- open hostname (show port)
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
  [IO (Maybe ())]
blastIt hostname ports = fmap (\p -> blast hostname p) ports

interfaces :: IO [NI.NetworkInterface]
interfaces = do
  NI.getNetworkInterfaces
