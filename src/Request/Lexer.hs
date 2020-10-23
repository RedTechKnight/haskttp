{-# LANGUAGE OverloadedStrings,LambdaCase #-}
module Request.Lexer where
import Hectoparsec
import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Void
import Data.Char
import Control.Applicative.Combinators
import Data.Functor

type RequestLexer = Parser ByteString Void String

data RequestToken =
  TWORD ByteString
  |TEOL deriving Show
run = evalParser pRequestToks
pRequestToks :: RequestLexer [RequestToken]
pRequestToks = some (try pWordTok <|> pEOLTok) <* endOfInput

pWordTok :: RequestLexer RequestToken
pWordTok = many pSpace
  *> (TWORD <$> tokenWhile1 ((not . flip elem [' ','\r','\n']) . chr . fromIntegral))
  <* (try (void $ some pSpace) <|> void (lookahead pEOLTok))

pEOLTok :: RequestLexer RequestToken
pEOLTok = TEOL <$ string "\r\n"

pSpace :: RequestLexer ()
pSpace = void $ char (fromIntegral . ord $ ' ')
