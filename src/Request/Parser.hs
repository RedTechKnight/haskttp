{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Request.Parser where

import Control.Applicative.Combinators (some, (<|>))
import Data.ByteString.Lazy (ByteString (..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Either (fromRight)
import Data.Map.Strict (Map (..))
import qualified Data.Map.Strict as Map
import Hectoparsec
  ( ErrorItem (ErrorItemLabels),
    MonadParser (lookahead, matchToken, try),
    ParseError,
    Parser,
    Pos (Pos),
    Stream (Chunk, Token, streamUncons, updatePosToken),
    Unexpected (UnexpectedEnd, UnexpectedToken),
    anyToken,
    evalParser,
  )
import Request.Lexer (RequestToken (..), pRequestToks)

type RequestParser = Parser [RequestToken] String String

data Request = Request Method ByteString B.ByteString Headers

type Headers = (Map ByteString ByteString)

data Method = GET | PUT | HEAD | POST | DELETE | OPTIONS deriving (Show, Ord, Eq)

instance Show Request where
  show (Request meth res v headers) =
    mconcat
      [ "Method: ",
        show meth,
        "\nResource: ",
        C.unpack res,
        "\nVersion: ",
        C.unpack v,
        show headers
      ]

parseRequest :: ByteString -> Either (ParseError [RequestToken] String String) Request
parseRequest = evalParser pRequest "" . fromRight [] . evalParser pRequestToks ""

pRequest :: RequestParser Request
pRequest = Request <$> pMethod <*> pWord <*> pWord <*> pHeaders

pHeaders :: RequestParser Headers
pHeaders = Map.delete "" . Map.fromList <$> collectHeaders
  where
    pHeader = (,) <$> pKey <*> (B.intercalate " " <$> some (try pWord)) <* pEOL
    collectHeaders =
      lookahead anyToken >>= \case
        TEOL -> pure []
        _ -> (:) <$> (try pHeader <|> pure ("", "")) <*> collectHeaders

pEOL :: RequestParser ()
pEOL = matchToken $ \case
  Just TEOL -> pure ()
  Nothing -> pure ()
  Just t -> Left (ErrorItemLabels (UnexpectedToken t) ["EOL"])

pMethod :: RequestParser Method
pMethod = matchToken $ \case
  Just (TWORD "GET") -> pure GET
  Just (TWORD "PUT") -> pure PUT
  Just (TWORD "HEAD") -> pure HEAD
  Just (TWORD "POST") -> pure POST
  Just (TWORD "DELETE") -> pure DELETE
  Just (TWORD "OPTIONS") -> pure OPTIONS
  Just t -> Left (ErrorItemLabels (UnexpectedToken t) ["Method"])
  Nothing -> Left (ErrorItemLabels UnexpectedEnd ["Method"])

pKey :: RequestParser ByteString
pKey = matchToken $ \case
  Just (TKEY w) -> pure w
  Just t -> Left (ErrorItemLabels (UnexpectedToken t) ["Key"])
  Nothing -> Left (ErrorItemLabels UnexpectedEnd ["Key"])

pWord :: RequestParser ByteString
pWord = matchToken $ \case
  Just (TWORD w) -> pure w
  Just t -> Left (ErrorItemLabels (UnexpectedToken t) ["Word"])
  Nothing -> Left (ErrorItemLabels UnexpectedEnd ["Word"])

instance Stream [a] where
  type Chunk [a] = [a]
  type Token [a] = a
  streamUncons [] = Nothing
  streamUncons (x : xs) = Just (x, xs)

  updatePosToken _ _ (Pos f l c) = Pos f l (c + 1)
