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
      (Just (Args 6666 "80, 443, 22, 21, 2222, 53" "capsulecorp.org" "misosoup" 2000000))

main = do
  (command : args) <- getArgs
  env <- getArgsEnv
  if command == "server"
    then do
      forkIO $ server TCP env
      server UDP env
    else pure ()

