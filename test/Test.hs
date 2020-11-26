{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative (Applicative (liftA2))
import Control.Arrow (ArrowChoice (left))
import Control.Concurrent.Async (async, uninterruptibleCancel)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State.Lazy (StateT (runStateT))
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map.Lazy as Map
import Hedgehog
  ( Gen,
    Group (Group),
    Property,
    checkParallel,
    forAll,
    property,
    success,
  )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Network.Socket
  ( Family (AF_INET),
    SockAddr (SockAddrInet),
    SocketType (Stream),
    bind,
    close,
    connect,
    defaultProtocol,
    listen,
    socket,
    tupleToHostAddress,
  )
import qualified Network.Socket.ByteString.Lazy as N
import Request.Parser
  ( Method (DELETE, GET, HEAD, OPTIONS, POST, PUT),
    Request (Request),
    parseRequest,
    prettyPrintRequest,
  )
import Server
    ( httpResponse,
      httpServer,
      responseInvalidRequest,
      responseMethUnsupported,
      responseNotFound,
      responseOk,
      HTTPServer(runHttpServer),
      Response(body, headers) )

main :: IO ()
main = do
  checkParallel
    ( Group
        "Tests"
        [ ("Pretty Printed Request Parses Properly", propParsesValidRequest)
        ]
    )
    >>= \case
      False -> error "Parser test failed!"
      True -> pure ()
  testHTTPServer

genRequest :: Gen Request
genRequest =
  Request
    <$> Gen.element [GET, PUT, POST, OPTIONS, DELETE, HEAD]
      <*> genBS
      <*> genBS
      <*> (Map.fromList <$> Gen.list (Range.linear 1 10) (liftA2 (,) genBS genBS))
  where
    genBS = C.pack <$> Gen.string (Range.linear 1 20) Gen.alphaNum

propParsesValidRequest :: Property
propParsesValidRequest = property $ do
  input <- forAll $ prettyPrintRequest <$> genRequest
  case parseRequest (C.pack input) of
    Left err -> error (show err <> ": " <> show input)
    Right _ -> success

testHTTPServer :: IO ()
testHTTPServer = do
  let (Left parseError) = left (C.pack . show) $ parseRequest "JUST random nonsense\n"
  testFileContent <- C.readFile "www/test.txt"

  sock <- socket AF_INET Stream defaultProtocol
  bind sock (SockAddrInet 4510 (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 10
  server <- async (flip runReaderT sock . flip runStateT [] . runHttpServer $ httpServer)
  testRequest
    "GET /test.txt HTTP/1.0\r\n\r\n"
    ( responseOk
        { body = testFileContent,
          headers =
            Map.fromList
              [ ("Server", "haskttp"),
                ("Content-Type", "text/plain"),
                ("Content-Length", C.pack . show . C.length $ testFileContent)
              ]
        }
    )
  testRequest "GET /nonexistant HTTP/1.0\r\n\r\n" responseNotFound
  testRequest "POST /shouldnt_work HTTP/1.0\r\n\r\n" responseMethUnsupported
  testRequest
    "JUST random nonsense\n"
    responseInvalidRequest
      { body = parseError,
        headers =
          Map.fromList
            [ ("Server", "haskttp"),
              ("Content-Type", "text/plain"),
              ("Content-Length", C.pack . show . C.length $ parseError)
            ]
      }
  uninterruptibleCancel server
  close sock

testRequest :: C.ByteString -> Response -> IO ()
testRequest request expectedResp = do
  sock <- socket AF_INET Stream defaultProtocol
  connect sock (SockAddrInet 4510 (tupleToHostAddress (127, 0, 0, 1)))
  N.send sock request
  resp <- N.recv sock 4096
  close sock
  if resp == httpResponse expectedResp
    then pure ()
    else error $ mconcat ["Expected response not received\nExpected: ", C.unpack . httpResponse $ expectedResp, "\nGot: ", C.unpack resp]