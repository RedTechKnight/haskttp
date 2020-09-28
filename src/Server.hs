{-# LANGUAGE OverloadedStrings,LambdaCase,ViewPatterns,GeneralizedNewtypeDeriving #-}
module Server where
import Network.Socket
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
import Request.Parser
import Hectoparsec


type Dictionary = Map.Map ByteString ByteString
newtype DictServer a = DictServer {unwrapDictServer :: ReaderT Socket (StateT Dictionary IO) a}
  deriving (Functor,Applicative,Monad,MonadReader Socket,MonadState Dictionary,MonadIO)

runDictServer :: DictServer ()
runDictServer = ask
  >>= liftIO . accept
  >>= \(conn,_) -> (fmap parseRequest . (liftIO . flip recv 4096) $ conn)
  >>= \case
    Right req -> local (const conn) (handleMessage req) >> liftIO (close conn) >> case req of
      Exit -> pure ()
      _ -> runDictServer
    Left err -> local (const conn) (respond (C.pack $ show (parseErrorItem err))) >> liftIO (close conn) >> runDictServer

handleMessage :: Request -> DictServer ()
handleMessage (Get key) = gets (Map.lookup key) >>= \case
  Nothing -> respond (fold ["ERROR key ",key," not found in dictionary.\n"]) >> pure ()
  Just value -> respond (fold ["ANSWER ",value,"\n"]) >> pure ()
handleMessage (Set key val) = modify (Map.insert key val) >> respond "SUCCESS Key inserted successfully.\n" >> pure ()
handleMessage Clear = modify (const Map.empty) >> respond "SUCCESS All dictionary entries erased.\n" >> pure ()
handleMessage (Remove key) = modify (Map.delete key) >> respond "SUCCESS Matching key removed from dictionary.\n" >> pure ()
handleMessage All =  gets (B.toLazyByteString . (B.lazyByteString "ANSWER Displaying all entries in dictionary:\n" <>) . foldMap (\(k,v) -> B.lazyByteString k <> ": " <> B.lazyByteString v <> "\n") . Map.toList) >>= respond >> pure ()
handleMessage Exit = respond "ENDED\n" >> pure ()

respond :: B.ByteString -> DictServer Int64
respond msg = ask >>= (liftIO . flip send msg) 

