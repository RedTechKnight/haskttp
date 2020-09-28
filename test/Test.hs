{-# LANGUAGE OverloadedStrings,ViewPatterns #-}
module Main where
import Server
import Network.Socket
import Network.Socket.ByteString.Lazy
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map.Lazy as Map
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Concurrent.Async

main :: IO ()
main = do
  runTestServer
 
runTestClient :: (Socket -> IO ()) -> IO ()
runTestClient f = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock (SockAddrInet 4510 (tupleToHostAddress (127,0,0,1)))
  f sock
  close sock

getTestAbsent :: IO ()
getTestAbsent = runTestClient $ \sock -> do
  send sock "GET absentword\n"
  resp <- recv sock 4096
  case resp of
    (C.isPrefixOf "ERROR" -> True) -> return ()
    _ -> error (show (ResultMismatch "ERROR" (C.takeWhile (/=' ') resp)))

getTestPresent :: IO ()
getTestPresent = runTestClient $ \sock -> do
  send sock "GET presentword\n"
  resp <- recv sock 4096
  case resp of
    (C.isPrefixOf "ANSWER" -> True) -> return ()
    _ -> error (show (ResultMismatch "ANSWER" (C.takeWhile (/=' ') resp)))

setTest :: IO ()
setTest = runTestClient $ \sock -> do
  send sock "SET newword a new word\n"
  resp <- recv sock 4096
  case resp of
    (C.isPrefixOf "SUCCESS" -> True) -> runTestClient $ \sock -> do
      send sock "GET newword\n"
      resp <- recv sock 4096
      case resp of
        (C.isPrefixOf "ANSWER" -> True) -> close sock >> return ()
        _ -> error (show (ResultMismatch "ANSWER" (C.takeWhile (/=' ') resp)))
 
    _ -> error (show (ResultMismatch "SUCESS" (C.takeWhile (/=' ') resp)))



exitTest :: IO ()
exitTest = runTestClient $ \sock -> do
  send sock "EXIT\n"
  return ()

runTestServer :: IO ()
runTestServer = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 4510 (tupleToHostAddress (127,0,0,1)))
  listen sock 1
  dictServer <- async $ flip runStateT (Map.singleton "presentword" "A word that is present") . flip runReaderT sock . unwrapDictServer $ runDictServer
  
  getTestAbsent
  getTestPresent
  setTest
  exitTest
  
  wait dictServer
  close sock
  
data TestError a =
  ResultMismatch a a

instance (Show a) => Show (TestError a) where
  show (ResultMismatch expect found) = mconcat ["Expected: ",show expect," --- Found: ",show found]
