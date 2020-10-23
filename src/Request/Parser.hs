{-# LANGUAGE OverloadedStrings, LambdaCase, ViewPatterns, TypeFamilies #-}
module Request.Parser (parseRequest)  where
import Request.Lexer
import Hectoparsec
import Data.Void
import Data.Either
import Control.Applicative.Combinators
import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

type RequestParser = Parser [RequestToken] String String

data Header = Header ByteString ByteString ByteString
instance Show Header where
  show (Header meth route v) = mconcat ["Method: ",C.unpack meth,
                                         "\nRoute: ",C.unpack route,
                                         "\nVersion: ",C.unpack v] 
parseRequest :: ByteString -> Header
parseRequest = fromRight (Header "NULL" "" "") . evalParser pHeader "" . fromRight [] . evalParser pRequestToks ""

pHeader :: RequestParser Header
pHeader = Header <$> pWord <*> pWord <*> pWord <* pEOL

pEOL :: RequestParser ()
pEOL = anyToken >>= \case
  TEOL -> pure ()
  _ -> empty

pWord :: RequestParser ByteString
pWord = anyToken >>= \case
  TWORD w -> pure w
  _ -> empty

instance Stream [a] where
  type Chunk [a] = [a]
  type Token [a] = a
  streamUncons [] = Nothing
  streamUncons (x:xs) = Just (x,xs)

  updatePosToken _ _ (Pos f l c) = Pos f l (c+1)
