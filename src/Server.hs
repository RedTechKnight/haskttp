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
import Control.Concurrent.MVar
import qualified Control.Concurrent.Async.Lifted as L
import Control.Concurrent.Async
import Control.Exception
import Request.Parser
import Hectoparsec


type Dictionary = Map.Map ByteString ByteString
newtype DictServer a = DictServer {runDictServer :: StateT [Async ()] (ReaderT (Socket,MVar Dictionary) IO) a}
  deriving (Functor,Applicative,Monad,MonadReader (Socket,MVar Dictionary),MonadState [Async ()],MonadIO)  

-- | Wait for a connection, accept it and serve the client in a seperate thread, wait for additional connections. 
run :: DictServer ()
run  = asks fst
  >>= liftIO . accept
  >>= \(conn,_) -> DictServer (L.async (runDictServer (local (bimap (const conn) id) serveClient)) >>= modify . (:) . fmap fst)
  >> get >>= liftIO . filterM (fmap (not . isJust) . poll) >>= put
  >> run

-- | Receive a request from the client, give the appropriate response, until the EXIT command is used.
serveClient :: DictServer ()
serveClient = 
  asks fst
  >>= (liftIO . flip recv 4096)
  >>= \req -> respond (C.pack . show $ parseRequest req) >> liftIO (print (show $ parseRequest req)) >> asks fst >>= liftIO . close 


respond :: B.ByteString -> DictServer Int64
respond msg = asks fst >>= (liftIO . flip send msg) 
