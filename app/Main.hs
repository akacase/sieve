module Main where

import Lib

main :: IO ()
main = do
  server 6666 TCP
