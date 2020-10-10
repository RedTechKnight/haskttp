{-# LANGUAGE OverloadedStrings,LambdaCase,ViewPatterns,GeneralizedNewtypeDeriving,MultiParamTypeClasses,TypeFamilies,FlexibleInstances,FlexibleContexts #-}
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
import Data.Int
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy as W hiding (All)
import Control.Monad.Trans.Control
import Control.Monad.Identity
import Control.Monad.Base
import Request.Parser
import Hectoparsec
import Control.Concurrent.MVar
import Control.Concurrent.Async.Lifted
import Control.Arrow


type Dictionary = Map.Map ByteString ByteString
newtype DictServer m a = DictServer {unwrapDictServer :: ReaderT (Socket,MVar Dictionary) m a}
  deriving (Functor,Applicative,Monad,MonadReader (Socket,MVar Dictionary),MonadTrans)  
instance MonadIO (DictServer IO) where
  liftIO action = DictServer (ReaderT $ \r -> action)
  
instance MonadBase IO (DictServer IO) where
  liftBase = liftBaseDefault

instance MonadTransControl DictServer where
  type StT DictServer a = a
  liftWith f = DictServer (ReaderT (\r -> f (\action -> runReaderT (unwrapDictServer action) r)))
  restoreT action = DictServer (ReaderT (const action))

instance MonadBaseControl IO (DictServer IO) where
  type StM (DictServer IO) a = a
  liftBaseWith f = DictServer (ReaderT (\r -> f (flip runReaderT r . unwrapDictServer)))
  restoreM = DictServer . return

runDictServer :: DictServer IO ()
runDictServer  = asks fst
  >>= liftIO . accept
  >>= \(conn,_) -> concurrently_ (local (const conn *** id) serveClient) runDictServer
  
serveClient :: DictServer IO ()
serveClient = liftIO (print "Serving client...")
  >> asks fst
  >>= fmap parseRequest . (liftIO . flip recv 4096)
  >>= \case
  Right req -> handleMessage req >> case req of
    Exit -> pure ()
    _ -> serveClient
  Left err -> respond (C.pack $ show (parseErrorItem err)) >> serveClient

handleMessage :: Request -> DictServer IO ()
handleMessage (Get key) = asks snd >>= liftIO . (flip withMVar (return . Map.lookup key)) >>= \case
  Nothing -> respond (fold ["ERROR key ",key," not found in dictionary.\n"]) >> pure ()
  Just value -> respond (fold ["ANSWER ",value,"\n"]) >> pure ()
handleMessage (Set key val) = asks snd
  >>= liftIO . (flip modifyMVar_ (return . Map.insert key val))
  >> respond "SUCCESS Key inserted successfully.\n"
  >> pure ()
handleMessage Clear = asks snd
  >>= liftIO . (flip modifyMVar_ (return . const Map.empty))
  >> respond "SUCCESS All dictionary entries erased.\n"
  >> pure ()
handleMessage (Remove key) = asks snd
  >>= liftIO . (flip modifyMVar_ (return . (Map.delete key)))
  >> respond "SUCCESS Matching key removed from dictionary.\n"
  >> pure ()
handleMessage All = asks snd
  >>= liftIO . (flip withMVar (return . (B.toLazyByteString . (B.lazyByteString "ANSWER Displaying all entries in dictionary:\n" <>) . foldMap (\(k,v) -> B.lazyByteString k <> ": " <> B.lazyByteString v <> "\n")) . Map.toList))
  >>= respond
  >> pure ()
handleMessage Exit = respond "ENDED\n" >> asks fst >>= liftIO . close >> pure ()

respond :: B.ByteString -> DictServer IO Int64
respond msg = asks fst >>= (liftIO . flip send msg) 

