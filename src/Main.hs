{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State.Lazy (StateT (runStateT))
import Network.Socket
  ( Family (AF_INET),
    SockAddr (SockAddrInet),
    SocketType (Stream),
    bind,
    close,
    defaultProtocol,
    listen,
    socket,
    tupleToHostAddress,
  )
import Server (HTTPServer (runHttpServer), httpServer)

main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 4510 (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 10
  flip runReaderT sock . flip runStateT [] . runHttpServer $ httpServer
  close sock
