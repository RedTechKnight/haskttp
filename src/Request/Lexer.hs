{-# LANGUAGE OverloadedStrings,LambdaCase #-}
module Request.Lexer where
import Hectoparsec
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Void
import Data.Char
import Control.Applicative.Combinators
import Data.Functor

type RequestLexer = Parser B.ByteString Void String

data RequestToken =
  TGET
  |TSET
  |TCLEAR
  |TREMOVE
  |TALL
  |TEXIT
  |TWORD B.ByteString
  |TINVALID Char
  deriving (Eq)

instance Show RequestToken where
  show TGET = "GET"
  show TSET = "SET"
  show TCLEAR = "CLEAR"
  show TREMOVE = "REMOVE"
  show TALL = "ALL"
  show TEXIT = "EXIT"
  show (TWORD bs) = C.unpack bs
  show (TINVALID c) = [c]

-- | Produces a list of tokens that a request is made up of, until the end of the input stream or a newline character is met. Any invalid characters in the sequence are passed on to the parser.
pRequestToks :: RequestLexer [RequestToken]
pRequestToks = some (try pCommandTok <|> try pWordTok <|> (TINVALID . chr . fromIntegral) <$> anyToken)

pWordTok :: RequestLexer RequestToken
pWordTok = pSpace
  *> (TWORD <$> tokenWhile1 ((not . flip elem [' ','\n']) . chr . fromEnum)) <* pStrip

pCommandTok :: RequestLexer RequestToken
pCommandTok = pSpace
              *> choice [TGET <$ string "GET"
                               ,TSET <$ string "SET"
                               ,TCLEAR <$ string "CLEAR"
                               ,TREMOVE <$ string "REMOVE"
                               ,TALL <$ string "ALL"
                               ,TEXIT <$ string "EXIT" ]
              <* (try pSpace1 <|> try pEOL <|> endOfInput) <* pStrip

pSpace1 :: RequestLexer ()
pSpace1 = void . tokenWhile1 $ (==' ') . toEnum . fromIntegral

pSpace :: RequestLexer ()
pSpace = void . tokenWhile $ (==' ') . toEnum . fromIntegral

pEOL :: RequestLexer ()
pEOL = void $ char (fromIntegral . fromEnum $ '\n')

pStrip :: RequestLexer ()
pStrip = pSpace <* many pEOL
