{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import Control.Concurrent.Async (Async, poll)
import Control.Concurrent.Async.Lifted (async)
import Control.Monad (filterM, void)
import Control.Monad.Reader
  ( MonadIO (..),
    MonadReader (ask, local),
    ReaderT,
  )
import Control.Monad.State.Lazy
  ( MonadState (get, put),
    StateT (StateT),
    modify,
  )
import Data.ByteString.Lazy (ByteString (..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Network.Socket (Socket, accept, close)
import qualified Network.Socket.ByteString.Lazy as N
import Request.Parser
  ( Method (GET),
    Request (..),
    parseRequest,
    showHeaders
  )
import System.IO.Error (tryIOError)
import Data.ByteString.Builder.Extra (defaultChunkSize)

newtype HTTPServer a = HTTPServer {runHttpServer :: StateT [Async ()] (ReaderT Socket IO) a}
  deriving (Functor, Applicative, Monad, MonadReader Socket, MonadState [Async ()], MonadIO)

-- | Wait for a connection, accept it and serve the client in a seperate thread, wait for additional connections. Current implmentation requires server to be terminated externally.
httpServer :: HTTPServer ()
httpServer = do
  (conn, _) <- ask >>= liftIO . accept
  launchClientThread conn
  removeCompletedThreads
  httpServer

removeCompletedThreads :: HTTPServer ()
removeCompletedThreads = get >>= liftIO . filterM (fmap isNothing . poll) >>= put

launchClientThread :: Socket -> HTTPServer ()
launchClientThread conn = do
  client <- HTTPServer $ async (runHttpServer (local (const conn) serveClient))
  modify ((fst <$> client) : )

recvAll :: Socket -> IO ByteString
recvAll sock = do
  bs <- N.recv sock $ fromIntegral defaultChunkSize
  if B.length bs < fromIntegral defaultChunkSize then 
    pure bs
  else 
    (bs <>) <$> recvAll sock

-- | Receive a request from the client and then give the appropriate response.
serveClient :: HTTPServer ()
serveClient = do
  sock <- ask
  reqBs <- liftIO . recvAll $ sock
  case parseRequest reqBs of
    Left err ->
      let message = C.pack . show $ err
          contentLen = C.length message
          hs =
            Map.fromList
              [ ("Server", "haskttp"),
                ("Content-Type", "text/plain"),
                ("Content-Length", C.pack . show $ contentLen)
              ]
       in (respond . httpResponse) responseInvalidRequest {body = message, headers = hs}
    Right req -> handleRequest req
  liftIO . close $ sock

handleRequest :: Request -> HTTPServer ()
handleRequest (Request GET file _ _) = do
  file <- liftIO . tryIOError . B.readFile . C.unpack $ (hostedFileDir <> file)
  case file of
    Left _ -> (respond . httpResponse) responseNotFound
    Right contents ->
      let hs =
            Map.fromList
              [ ("Server","haskttp"),
                ("Content-Type", "text/plain"),
                ("Content-Length", C.pack . show . C.length $ contents)
              ]
       in (respond . httpResponse) responseOk {headers = hs, body = contents}
handleRequest _ = (respond . httpResponse) responseMethUnsupported

respond :: ByteString -> HTTPServer ()
respond msg = ask >>= void . liftIO . flip N.send msg

hostedFileDir :: ByteString
hostedFileDir = "www"

data Response = Response
  { version :: ByteString,
    statusCode :: Int,
    statusDesc :: ByteString,
    headers :: Map.Map ByteString ByteString,
    body :: ByteString
  }

httpResponse :: Response -> ByteString
httpResponse (Response vers status statusDesc headers body) =
  vers <> " "
    <> (C.pack . show $ status)
    <> " "
    <> statusDesc
    <> "\r\n"
    <> showHeaders headers
    <> "\r\n"
    <> body
    <> "\r\n"

responseMethUnsupported :: Response
responseMethUnsupported = Response "HTTP/1.0" 501 "Not Implemented" Map.empty "The method used is not supported by the server."

responseInvalidRequest :: Response
responseInvalidRequest = Response "HTTP/1.0" 400 "Bad Request" Map.empty "The request received was ill-formed."

responseNotFound :: Response
responseNotFound = Response "HTTP/1.0" 404 "Not Found" Map.empty "The requested resource could not be located."

responseOk :: Response
responseOk = Response "HTTP/1.0" 200 "Ok" Map.empty ""

