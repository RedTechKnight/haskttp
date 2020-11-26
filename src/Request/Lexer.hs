{-# LANGUAGE OverloadedStrings #-}

module Request.Lexer where

import Control.Applicative.Combinators
  ( many,
    optional,
    some,
    (<|>),
  )
import Data.ByteString.Lazy (ByteString (..))
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char (chr, ord)
import Data.Functor (void)
import Data.Void (Void)
import Hectoparsec
  ( MonadParser (endOfInput, lookahead, try),
    Parser,
    char,
    string,
    tokenWhile1,
    anyToken
  )

type RequestLexer = Parser ByteString Void String

data RequestToken
  = TWORD ByteString
  | TKEY ByteString
  | TEOL
  | TINVAL
  deriving (Eq, Show)

pRequestToks :: RequestLexer [RequestToken]
pRequestToks = some (try pKeyTok <|> try pWordTok <|> try pEOLTok <|> pInvalTok)

pInvalTok :: RequestLexer RequestToken
pInvalTok = TINVAL <$ anyToken

pKeyTok :: RequestLexer RequestToken
pKeyTok =
  many pSpace
    *> (TKEY <$> tokenWhile1 ((not . flip elem [' ', '\r', '\n', ':']) . chr . fromIntegral))
    <* char (fromIntegral . ord $ ':')
    <* (void . some $ pSpace)

pWordTok :: RequestLexer RequestToken
pWordTok =
  many pSpace
    *> (TWORD <$> tokenWhile1 ((not . flip elem [' ', '\r', '\n']) . chr . fromIntegral))
    <* (try (void $ some pSpace) <|> try (void (lookahead pEOLTok)) <|> endOfInput)

pEOLTok :: RequestLexer RequestToken
pEOLTok = TEOL <$ (optional (string "\r") *> string "\n")

pSpace :: RequestLexer ()
pSpace = void $ char (fromIntegral . ord $ ' ')

prettyPrintTok :: RequestToken -> String
prettyPrintTok TEOL = "\r\n"
prettyPrintTok (TWORD w) = C.unpack w
prettyPrintTok (TKEY w) = C.unpack w <> ":"
