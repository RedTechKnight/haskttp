{-# LANGUAGE OverloadedStrings,LambdaCase,ViewPatterns,GeneralizedNewtypeDeriving #-}
module Main where
import Prelude hiding (getContents)
import Network.Socket
import Network.Socket.ByteString.Lazy
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map.Lazy as Map
import System.IO.Error
import Data.Foldable
import Data.Bifunctor
import Data.Tuple
import Data.Int
import Control.Concurrent
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Lazy
main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 4510 (tupleToHostAddress (127,0,0,1)))
  listen sock 1
  (conn,addr) <- accept sock
  flip runStateT Map.empty . flip runReaderT conn . unwrapDictServer $ runDictServer
  close sock

type Dictionary = Map.Map B.ByteString B.ByteString
newtype DictServer a = DictServer {unwrapDictServer :: ReaderT Socket (StateT Dictionary IO) a}
  deriving (Functor,Applicative,Monad,MonadReader Socket,MonadState Dictionary,MonadIO)

runDictServer :: DictServer ()
runDictServer = ask >>= fmap spanOnSpace . (liftIO . flip recv 4096) >>= uncurry handleMessage

handleMessage :: B.ByteString -> B.ByteString -> DictServer ()
handleMessage "GET" (C.takeWhile (/=' ') -> key) = gets (Map.lookup key) >>= \case
  Nothing -> respond (fold ["ERROR key ",key," not found in dictionary.\n"]) >> runDictServer
  Just value -> respond (fold ["ANSWER ",value,"\n"]) >> runDictServer
handleMessage "SET" (spanOnSpace -> ("",_)) = respond "ERROR First parameter is empty.\n" >> runDictServer
handleMessage "SET" (spanOnSpace -> (_,"")) = respond "ERROR Second parameter is empty.\n" >> runDictServer
handleMessage "SET" (spanOnSpace -> (key,val)) = modify (Map.insert key val) >> respond "SUCCESS Key inserted successfully.\n" >> runDictServer
handleMessage "CLEAR" _ = modify (const Map.empty) >> respond "SUCCESS All dictionary entries erased.\n" >> runDictServer
handleMessage "REMOVE" (C.takeWhile (/=' ') -> key) = modify (Map.delete key) >> respond "SUCCESS Matching key removed from dictionary.\n" >> runDictServer
handleMessage "ALL" _ =  gets (B.toLazyByteString . (B.lazyByteString "ANSWER Displaying all entries in dictionary:\n" <>) . foldMap (\(k,v) -> B.lazyByteString k <> ": " <> B.lazyByteString v <> "\n") . Map.toList) >>= respond >> runDictServer
handleMessage "EXIT" _ = respond "ENDED" >> pure ()
handleMessage comm _ = respond (fold ["ERROR Unrecognised command ",comm," received.\n"]) >> runDictServer

respond :: B.ByteString -> DictServer Int64
respond msg = ask >>= (liftIO . flip send msg) 

spanOnSpace :: B.ByteString -> (B.ByteString,B.ByteString)
spanOnSpace = (C.takeWhile (/=' ') &&& C.drop 1 . C.dropWhile (/=' ')) . C.takeWhile (/='\n')
