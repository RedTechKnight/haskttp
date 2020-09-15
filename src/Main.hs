{-# LANGUAGE OverloadedStrings,LambdaCase,ViewPatterns #-}
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
import Control.Concurrent
import Control.Applicative
import Control.Monad
main :: IO ()
main = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 4510 (tupleToHostAddress (127,0,0,1)))
  listen sock 1
  (conn,addr) <- accept sock
  runDictServer conn Map.empty
  close sock

runDictServer :: Socket -> Map.Map B.ByteString B.ByteString -> IO ()
runDictServer clientConn map = second (C.dropWhile (==' ')) . C.break (==' ') . C.takeWhile (/='\n') <$> recv clientConn 4096 >>= \case
  ("GET",key) -> case Map.lookup (C.takeWhile (/=' ') $ key) map of
    Nothing -> send clientConn (fold ["ERROR key ",key,",not found in dictionary.\n"]) >> runDictServer clientConn map
    Just value -> send clientConn (fold ["ANSWER ",value,"\n"]) >> runDictServer clientConn map
  ("SET",args) -> let key = C.takeWhile (/=' ') args
                      value = C.takeWhile (/= '\n') . C.drop 1 . C.dropWhile (/=' ') $ args
                  in case (key,value) of
                       ("",_) -> send clientConn "ERROR No key specified.\n" >> runDictServer clientConn map
                       (_,"") -> send clientConn "ERROR No value specified.\n" >> runDictServer clientConn map
                       (key,value) -> send clientConn (fold ["SUCCESS Set key ",key," to value ",value,"\n"]) >> runDictServer clientConn (Map.insert key value map)
  ("CLEAR",_) -> send clientConn "SUCCESS clearing all contents." >> runDictServer clientConn Map.empty
  ("ALL",_) -> let kvs = foldMap (\(k,v) -> k <> ": " <> v <> "\n") $ Map.toList map in send clientConn ("ANSWER Presenting definitions of all words stored:\n" <> kvs) >> runDictServer clientConn map
  ("EXIT",_) -> send clientConn "FINISHED" >> close clientConn
  (comm,_) -> send clientConn (fold ["ERROR Unrecognised command ",comm," received. Ignoring.\n"]) >> runDictServer clientConn map
 
