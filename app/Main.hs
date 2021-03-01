{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Concurrent (forkIO)
import GHC.Generics
import Lib
import System.Environment
import System.Envy

getArgsEnv :: IO (Either String Args)
getArgsEnv =
  runEnv $
    gFromEnvCustom
      defOption
      (Just (Args 6666 "80, 443, 22, 21, 2222, 6666, 53" "127.0.0.1" "misosoup" "sieve" 2000000))


main :: IO ()
main = do
  (command : args) <- getArgs
  env <- getArgsEnv
  case env of
    Left error -> putStrLn error
    Right e ->
      -- TODO: start file handle at top-level and pass it into `server`
      -- TODO: add UDP/TCP identifiers and hostname to client
      if command == "server"
        then do
          forkIO $ server TCP e
          server UDP e
        else
          if command == "blast"
            then do
              if (head args) == "tcp"
                then do
                  blastIt e TCP
                else blastIt e UDP
              pure ()
            else pure ()



