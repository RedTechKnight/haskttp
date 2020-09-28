{-# LANGUAGE OverloadedStrings, LambdaCase, TypeFamilies #-}
module Request.Parser (parseRequest,Request(..))  where
import Request.Lexer
import Hectoparsec
import Data.Void
import Data.Either
import Control.Applicative.Combinators
import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
type RequestParser = Parser [RequestToken] RequestParserError String

data Request =
  Get B.ByteString
  |Set B.ByteString B.ByteString
  |Clear
  |Remove B.ByteString
  |All
  |Exit
  |BadReq deriving Show

data RequestParserError =
  RequestParserError {
  expected :: Maybe RequestToken
  ,found :: Maybe RequestToken
                     }

instance Show RequestParserError where
  show (RequestParserError expected found) = mconcat ["Expected: ",present expected," --- Found: ",present found]
    where
      present Nothing = "End of Input"
      present x = show x

parseRequest :: ByteString -> Either (ParseError [RequestToken] RequestParserError String) Request
parseRequest input = evalParser pRequest "" . fromRight [] $ evalParser pRequestToks "" input 
  
pRequest :: RequestParser Request
pRequest = choice [pGet
                  ,pSet
                  ,pClear
                  ,pRemove
                  ,pAll
                  ,pExit]

pCommand commTok = (satisfy (==commTok))
  <|> try (pError (Just commTok))

pGet :: RequestParser Request
pGet = pCommand TGET
  *> (Get <$> pWord) <* pEnd

pSet :: RequestParser Request
pSet = pCommand TSET
  *> (Set <$> pWord <*> pWords <* pEnd)

pClear :: RequestParser Request
pClear = Clear <$ pCommand TCLEAR <* pEnd

pRemove :: RequestParser Request
pRemove = pCommand TREMOVE
  *> (Remove <$> pWord) <* pEnd

pAll :: RequestParser Request
pAll = All <$ pCommand TALL <* pEnd

pExit :: RequestParser Request
pExit = Exit <$ pCommand TEXIT <* pEnd

pWord :: RequestParser ByteString
pWord = flip (<|>) (pError $ Just (TWORD "*")) $ try anyToken >>= \case
  TWORD w -> pure w
  s -> customError $ RequestParserError (Just $ TWORD "*") (Just s)

pWords :: RequestParser ByteString
pWords = B.intercalate " " <$> (some pWord <|> pError (Just $ TWORD "*"))

pEnd :: RequestParser ()
pEnd = endOfInput <|> pError Nothing

pError :: Maybe RequestToken -> RequestParser a
pError expect = atEnd >>= \case
  False -> anyToken >>= \found -> customError $ RequestParserError expect (Just found)
  True -> customError $ RequestParserError expect Nothing

instance Stream [a] where
  type Chunk [a] = [a]
  type Token [a] = a
  streamUncons [] = Nothing
  streamUncons (x:xs) = Just (x,xs)

  updatePosToken _ _ (Pos f l c) = Pos f l (c+1)
