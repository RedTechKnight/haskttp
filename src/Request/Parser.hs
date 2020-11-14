
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
import Data.Map.Strict (Map(..))
import qualified Data.Map.Strict as Map

type RequestParser = Parser [RequestToken] String String
data Request = Request Method ByteString B.ByteString Headers
data BadRequest = InvalidMethod
  |InvalidResource
  |InvalidVersion
  |ParsingFailure
  
data Headers = Headers (Map ByteString ByteString)

data Method = GET|PUT|HEAD|POST|DELETE|OPTIONS deriving (Show,Ord,Eq)

  
instance Show Headers where
  show (Headers m) = foldMap (\(k,v) -> mconcat [C.unpack k,": ",C.unpack v,"\r\n"]) (Map.toList m)
instance Show Request where
  show (Request meth res v headers) = mconcat ["Method: ",show meth
                                              ,"\nResource: ",C.unpack res
                                              , "\nVersion: ",C.unpack v
                                              ,show headers]

parseRequest :: ByteString -> Either BadRequest Request
parseRequest input = case evalParser pRequest "" . fromRight [] $ evalParser pRequestToks "" input of
  Left _ -> Left ParsingFailure
  Right req -> req

pRequest :: RequestParser (Either BadRequest Request)
pRequest = optional pMethod >>= \case
  Just meth -> optional pWord >>= \case
    Just res -> optional pWord >>= \case
      Just v -> pHeaders >>= \h -> pure $ Right (Request meth res v h) 
      Nothing -> pure $ Left InvalidVersion
    Nothing -> pure $ Left InvalidResource
  Nothing -> pure $ Left InvalidMethod
  

pHeaders :: RequestParser Headers
pHeaders = Headers . Map.fromList . filter (==("","")) <$> collectHeaders where
  collectHeaders = do
    header <- (try ((,) <$> pKey <*> (B.intercalate " " <$> some (try pWord)))) <|> pure ("","") <* pEOL
    lookahead anyToken >>= \case
      TEOL -> return [header]
      _ -> (header:) <$> collectHeaders

pEOL :: RequestParser ()
pEOL = try endOfInput <|> (anyToken >>= \case
  TEOL -> pure ()
  t -> unexpected (UnexpectedToken t) ["EOL"])

pMethod :: RequestParser Method
pMethod = try (anyToken >>= \case
  TWORD "GET" -> pure GET
  TWORD "PUT" -> pure PUT
  TWORD "HEAD" -> pure HEAD
  TWORD "POST" -> pure POST
  TWORD "DELETE" -> pure DELETE
  TWORD "OPTIONS" -> pure OPTIONS
  other -> unexpected (UnexpectedToken other) ["Method"]) <|> 
  unexpected UnexpectedEnd ["Method"]

pKey :: RequestParser ByteString
pKey =  matchToken $ \case
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
  streamUncons (x:xs) = Just (x,xs)

  updatePosToken _ _ (Pos f l c) = Pos f l (c+1)
