module Lib
    ( someFunc
    ) where
import Foreign.C.Types ( CInt ) 
import System.Timeout ( timeout )
import qualified Data.ByteString.Char8 as C
import Data.Bits
import Network.Socket.Address( sendAllTo )
import Network.Socket
    ( HostName,
      SockAddr,
      Socket,
      AddrInfo(addrFamily, addrAddress, addrSocketType, addrProtocol),
      getAddrInfo,
      connect,
      close,
      defaultProtocol,
      socket, SocketOption (SendTimeOut, UserTimeout), setSocketOption )
import Network.BSD ( HostName, defaultProtocol )
import Data.List ( genericDrop )
import Network.Socket.Address (sendTo)
import Foreign.C

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let addr = head addrinfos

    -- Establish a socket for communication
    sock <- Network.Socket.socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    --connect sock (addrAddress addr)

    -- Save off the socket, program name, and server address in a handle
    return $ Handle sock (addrAddress addr)

  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.

send :: String -> IO Handle -> IO ()
send message handler = do
  h <- handler
  sendAllTo 
    (sock h)
    (C.pack message)
    (address h)
  Network.Socket.close (sock h)

blast :: String -> IO Handle -> IO (Maybe ())
blast message handler = do
  timeout 2000000 $ send message handler

close :: Handle -> IO ()
close handler = Network.Socket.close (sock handler)
