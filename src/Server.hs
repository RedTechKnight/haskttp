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
  >>= fmap parseRequest . (liftIO . flip recv 4096)
  >>= \req -> handleRequest req >> case req of
    Exit -> pure ()
    _ -> serveClient

-- | Performs the appropriate action based on the request received.
handleRequest :: Request -> DictServer ()
handleRequest (Get key) = asks snd >>= liftIO . (flip withMVar (return . Map.lookup key)) >>= \case
  Nothing -> respond (fold ["ERROR key ",key," not found in dictionary.\n"]) >> pure ()
  Just value -> respond (fold ["ANSWER ",value,"\n"]) >> pure ()
handleRequest (Set key val) = asks snd
  >>= liftIO . (flip modifyMVar_ (return . Map.insert key val))
  >> respond "SUCCESS Key inserted successfully.\n"
  >> pure ()
handleRequest Clear = asks snd
  >>= liftIO . (flip modifyMVar_ (return . const Map.empty))
  >> respond "SUCCESS All dictionary entries erased.\n"
  >> pure ()
handleRequest (Remove key) = asks snd
  >>= liftIO . (flip modifyMVar_ (return . (Map.delete key)))
  >> respond "SUCCESS Matching key removed from dictionary.\n"
  >> pure ()
handleRequest All = asks snd
  >>= liftIO . (flip withMVar (return . showAllDictContents))
  >>= respond
  >> pure ()
handleRequest Exit =
  respond "ENDED\n"
  >> asks fst
  >>= liftIO . close
  >> pure ()
handleRequest EmptyRequest =
  respond "ERROR request empty or could not be parsed successfully\n"
  >> pure ()
handleRequest req@(BadRequest _ _) =
  respond (reportBadReqError req)
  >> pure ()
respond :: B.ByteString -> DictServer Int64
respond msg = asks fst >>= (liftIO . flip send msg) 

showAllDictContents :: Map ByteString ByteString -> ByteString
showAllDictContents map = (joinStrings $ "ANSWER Displaying all entries in dictionary:" : fmap showPair pairs) <> "\n"
  where
    pairs = Map.toList map
    showPair (key,val) = key <> ": " <> val
    joinStrings = B.intercalate "\n"
  
