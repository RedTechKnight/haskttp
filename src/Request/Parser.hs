
{-# LANGUAGE OverloadedStrings, LambdaCase, ViewPatterns, TypeFamilies #-}
module Request.Parser   where
import Request.Lexer
import Hectoparsec
import Data.Void
import Data.Either
import Control.Applicative.Combinators
import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

type RequestParser = Parser [RequestToken] String String
data Request = Request ByteString ByteString ByteString [Header]
data Header = Header ByteString ByteString
instance Show Header where
  show (Header k v) = mconcat [C.unpack k,": ",C.unpack v]
instance Show Request where
  show (Request meth res v headers) = mconcat ["Method: ",C.unpack meth
                                              ,"\nResource: ",C.unpack res
                                              , "\nVersion: ",C.unpack v
                                              ,foldMap (("\n  "++) . show) headers]

parseRequest :: ByteString -> Request
parseRequest = fromRight (Request "NULL" "" "" []) . evalParser (pRequest <*> pHeaders) "" . fromRight [] . evalParser pRequestToks ""

pRequest :: RequestParser ([Header] -> Request)
pRequest = label "pRequest" (Request <$> pWord <*> pWord <*> pWord <* pEOL)

pHeaders :: RequestParser [Header]
pHeaders = some pHeader

pHeader :: RequestParser Header
pHeader = (label "pHeader" (Header <$> pKey <*> (B.intercalate " " <$> some (try pWord)) <* pEOL))

pEOL :: RequestParser ()
pEOL = label "pEnd" (try (anyToken >>= \case
  TEOL -> pure ()
  _ -> empty) <|> endOfInput)

pKey :: RequestParser ByteString
pKey = label "pKey" $ anyToken >>= \case
  TKEY k -> pure k
  _ -> empty

pWord :: RequestParser ByteString
pWord = label "pWord" $ anyToken >>= \case
  TWORD w -> pure w
  _ -> empty

instance Stream [a] where
  type Chunk [a] = [a]
  type Token [a] = a
  streamUncons [] = Nothing
  streamUncons (x:xs) = Just (x,xs)

  updatePosToken _ _ (Pos f l c) = Pos f l (c+1)
