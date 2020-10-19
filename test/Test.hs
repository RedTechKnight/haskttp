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
import Control.Concurrent.MVar
import Request.Lexer
import Request.Parser
import Hectoparsec
import Data.Either
import Test.QuickCheck

main :: IO ()
main = do
  quickCheck noLeftReturnedRequestParser
  quickCheck noLeftReturnedRequestLexer
  runTestServer


runTestClient :: B.ByteString -> (B.ByteString -> Bool) -> (B.ByteString -> String) -> IO ()
runTestClient request responsePred makeErr = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock (SockAddrInet 4510 (tupleToHostAddress (127,0,0,1)))
  send sock request
  resp <- recv sock 4096
  case resp of
    (responsePred -> True) -> return ()
    resp -> error $ makeErr resp
  send sock "EXIT\n"
  _ <- recv sock 4096
  close sock

runTestServer :: IO ()
runTestServer = do
  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 4510 (tupleToHostAddress (127,0,0,1)))
  dict <- newMVar (Map.singleton "presentword" "A word that is present")
  listen sock 1
  dictServer <- async . flip runReaderT (sock,dict) . flip runStateT [] . runDictServer $ run

  runTestClient
    "GET absentword\n"
    (C.isPrefixOf "ERROR")
    (show . ResultMismatch "ERROR" . (C.takeWhile (/=' ')))
  runTestClient
    "GET presentword\n"
    (C.isPrefixOf "ANSWER")
    (show . ResultMismatch "ANSWER" . (C.takeWhile (/=' ')))
  runTestClient
    "SET newword something new\n"
    (C.isPrefixOf "SUCCESS")
    (show . ResultMismatch "SUCESS" . (C.takeWhile (/=' ')))
  runTestClient
    "GET newword\n"
    (C.isPrefixOf "ANSWER")
    (show . ResultMismatch "ANSWER" . (C.takeWhile (/=' ')))
  runTestClient
    "REMOVE newword\n"
    (C.isPrefixOf "SUCCESS")
    (show . ResultMismatch "SUCESS" . (C.takeWhile (/=' ')))
  runTestClient
    "GET newword\n"
    (C.isPrefixOf "ERROR")
    (show . ResultMismatch "ERROR" . (C.takeWhile (/=' ')))
  runTestClient
    "SET wordA a word\n"
    (C.isPrefixOf "SUCCESS")
    (show . ResultMismatch "SUCCESS" . (C.takeWhile (/=' ')))
  runTestClient
    "SET wordB another word\n"
    (C.isPrefixOf "SUCCESS")
    (show . ResultMismatch "SUCCESS" . (C.takeWhile (/=' ')))
  runTestClient
    "SET wordC yet another word\n"
    (C.isPrefixOf "SUCCESS")
    (show . ResultMismatch "SUCCESS" . (C.takeWhile (/=' ')))
  runTestClient
    "ALL\n"
    (allDefined ["presentword","wordA","wordB","wordC"] . filter (not . C.null) . fmap (C.takeWhile (/=':')). drop 1
     . C.split '\n')
    (show . ResultMismatch "Various definitions")
  runTestClient
    "CLEAR\n"
    (C.isPrefixOf "SUCCESS")
    (show . ResultMismatch "SUCESS" . C.takeWhile (/=' '))
  runTestClient
    "ALL\n"
    (null . filter (not . C.null) . drop 1 . C.split '\n')
    (\_ -> show $ ResultMismatch "Empty dictionary" "various definitions")
  
  cancel dictServer
  close sock

allDefined :: [B.ByteString] -> [B.ByteString] -> Bool
allDefined as bs = if length as == length bs
  then and . fmap (uncurry (==)) $ zip as bs
  else False

data TestError a =
  ResultMismatch a a

instance (Show a) => Show (TestError a) where
  show (ResultMismatch expect found) = mconcat ["Expected: ",show expect," --- Found: ",show found]

noLeftReturnedRequestParser :: [RequestToken] -> Bool
noLeftReturnedRequestParser = isRight . evalParser pRequest ""

noLeftReturnedRequestLexer :: String -> Bool
noLeftReturnedRequestLexer = isRight . evalParser pRequestToks "" . C.pack

instance Arbitrary RequestToken where
  arbitrary = oneof [return TGET,return TSET,return TCLEAR,return TREMOVE,return TALL,return TEXIT,(TWORD . C.pack) <$> arbitrary, TINVALID <$> arbitrary]
  
