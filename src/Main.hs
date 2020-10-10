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
  listen sock 1
  dict <- newMVar (Map.singleton "word" "Just some letters")
  flip runReaderT (sock,dict) . flip runStateT [] .  runDictServer $ run
  close sock
