{-# LANGUAGE OverloadedStrings,LambdaCase,ViewPatterns,GeneralizedNewtypeDeriving #-}
module Main where
import Network.Socket
import qualified Data.Map.Lazy as Map
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy hiding (listen)
import Server
import Control.Concurrent.MVar
import Control.Concurrent.Async

main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 4510 (tupleToHostAddress (127,0,0,1)))
  listen sock 10
  flip runReaderT sock . flip runStateT [] .  runHttpServer $ httpServer
  close sock
