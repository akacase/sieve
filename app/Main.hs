module Main where

import Lib
import System.Environment   
import Control.Concurrent (forkIO)

main = do
  (command : args) <- getArgs
  if command == "server"
    then do
      forkIO $ server 6666 TCP
      server 6666 UDP
    else 
      pure()



