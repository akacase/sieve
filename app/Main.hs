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
      (Just (Args 6666 "80, 443, 22, 21, 2222, 6666, 53" "127.0.0.1" "misosoup" "sieve.json" 2000000))

main = do
  (command : args) <- getArgs
  env <- getArgsEnv
  case env of
    Left error -> putStrLn error
    Right e ->
      if command == "server"
        then do
          forkIO $ server TCP e
          server UDP e
        else
          if command == "blast"
            then do
              if (head args) == "tcp"
                then do
                  head $ blastIt e TCP
                else head $ blastIt e UDP
              pure ()
            else pure ()



