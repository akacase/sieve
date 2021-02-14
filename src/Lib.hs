module Lib
    ( someFunc
    ) where
import Foreign.C.Types ( CInt ) 
import System.Timeout ( timeout )
import qualified Data.ByteString.Char8 as C
import Data.Bits
import Network.Socket.Address( sendAllTo )
import Network.Socket.ByteString (recv, sendAll)
import Network.Socket
    ( HostName,
      SockAddr,
      Socket,
      Family ( AF_INET ),
      SocketType ( Stream, Raw ),
      AddrInfo(addrFamily, addrAddress, addrSocketType, addrProtocol),
      getAddrInfo,
      defaultHints,
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
    let hints = defaultHints { addrSocketType = Stream, addrFamily = AF_INET }
    addrinfos <- getAddrInfo (Just defaultHints) (Just hostname) (Just port)
    let addr = head addrinfos

    -- Establish a socket for communication
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock (addrAddress addr)
    -- Save off the socket, program name, and server address in a handle
    return $ Handle sock (addrAddress addr)

  -- Now you may use connectionSocket as you please within this scope,
  -- possibly using recv and send to interact with the remote end.

send :: String -> Handle -> IO ()
send message handler = do
  sendAll
    (sock handler)
    (C.pack message)
  Network.Socket.close (sock handler)

blast :: String -> String -> Int -> IO (Maybe ())
blast hostname message port = do
  h <- open hostname (show port)
  -- timeout of 2 seconds
  timeout 2000000 $ send message h

close :: Handle -> IO ()
close handler = Network.Socket.close (sock handler)

-- TODO: sequence_ $ blastIt "capsulecorp.org" "watup" [ 1 .. 4000 ]
-- use that style on main, also, allow other protocols (UDP)
blastIt :: 
  -- | Remote hostname, or localhost
  String -> 
  -- | Message to send to host
  String -> 
  -- | Series of ports, i.e. [ 1.. 400 ] or the defined `ports` variable
  [Int] -> [IO (Maybe ())]
blastIt hostname message ports = fmap (\p -> blast hostname message p) ports


