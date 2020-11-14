{-# LANGUAGE OverloadedStrings,LambdaCase,ViewPatterns,GeneralizedNewtypeDeriving#-}
module Server where
import Network.Socket hiding (listen)
import Network.Socket.ByteString.Lazy
import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Map (Map(..))
import qualified Data.Map as Map
import Data.Foldable
import Data.Either
import Data.Maybe
import Data.Bifunctor
import Data.Int
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy hiding (All)
import Control.Monad.Trans.Control
import Control.Monad.Identity
import Control.Monad.Base
import qualified Control.Concurrent.Async.Lifted as L
import Control.Concurrent.Async
import Control.Exception
import System.IO.Error
import Request.Lexer
import Request.Parser
import Hectoparsec

newtype HTTPServer a = HTTPServer {runHttpServer :: StateT [Async ()] (ReaderT Socket IO) a}
  deriving (Functor,Applicative,Monad,MonadReader Socket,MonadState [Async ()],MonadIO)

-- | Wait for a connection, accept it and serve the client in a seperate thread, wait for additional connections.
httpServer :: HTTPServer ()
httpServer  = ask
  >>= liftIO . accept
  >>= \(conn,_) -> HTTPServer (L.async (runHttpServer (local (const conn) serveClient)) >>= modify . (:) . fmap fst)
  >> get >>= liftIO . filterM (fmap (not . isJust) . poll) >>= put
  >> httpServer

-- | Receive a request from the client and then give the appropriate response.
serveClient :: HTTPServer ()
serveClient =
  ask
  >>= (liftIO . flip recv 4096)
  >>= \req -> (case parseRequest req of
                Left _ -> (respond . httpResponse) responseInvalidRequest
                Right req -> handleRequest req)
  >> ask >>= liftIO . close

handleRequest :: Request -> HTTPServer ()
handleRequest (Request GET file _ _) = (liftIO . tryIOError . B.readFile . C.unpack . B.append hostFileDir $ file) >>= \case
  Left _ -> (respond . httpResponse) responseNotFound
  Right contents -> (respond . httpResponse) $
    Response
    "HTTP/1.0"
    200
    "Ok"
    (Headers (Map.fromList
              [("Content-Length",C.pack . show . B.length $ contents)
              ,("Content-Type","text/plain")
              ,("Server","haskttp")]))
    contents
handleRequest (Request HEAD file _ _) = (liftIO . tryIOError . B.readFile . C.unpack . B.append hostFileDir $ file) >>= \case
  Left _ -> let (Response vers status desc headers _) = responseNotFound in (respond . httpResponse) (Response vers status desc headers "")
  Right contents -> (respond . httpResponse) $
    Response
    "HTTP/1.0"
    200
    "Ok"
    (Headers (Map.fromList
              [("Content-Length",C.pack . show . B.length $ contents)
              ,("Content-Type","text/plain")
              ,("Server","haskttp")]))
    ""
handleRequest _ = (respond . httpResponse) responseMethUnsupported 

respond :: ByteString -> HTTPServer ()
respond msg = ask >>= (void . liftIO . flip send msg)

hostFileDir = "www"

data Response = Response
  ByteString
  Int
  ByteString
  Headers
  ByteString

httpResponse :: Response -> ByteString
httpResponse (Response vers status statusDesc headers body) =
  vers <> " "
  <> (C.pack . show $ status) <> " "
  <> statusDesc <> "\r\n"
  <> (C.pack . show $ headers) <> "\r\n"
  <> body <> "\r\n"


responseMethUnsupported = Response "HTTP/1.0" 501 "Not Implemented" (Headers Map.empty) "The method used is not supported by the server."
responseInvalidRequest = Response "HTTP/1.0" 400 "Bad Request" (Headers Map.empty) "The request received was ill-formed."
responseNotFound = Response "HTTP/1.0" 404 "Not Found" (Headers Map.empty) "The requested resource could not be located."
