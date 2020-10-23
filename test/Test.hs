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
main = pure ()
