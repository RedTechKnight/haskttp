{-# LANGUAGE OverloadedStrings, LambdaCase, ViewPatterns, TypeFamilies #-}
module Request.Parser (reportBadReqError
                      ,pRequest
                      ,parseRequest
                      ,Request(..))  where
import Request.Lexer
import Hectoparsec
import Data.Void
import Data.Either
import Control.Applicative.Combinators
import Data.ByteString.Lazy (ByteString(..))
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

type RequestParser = Parser [RequestToken] String String

data Request =
  Get B.ByteString
  |Set B.ByteString B.ByteString
  |Clear
  |Remove B.ByteString
  |All
  |Exit
  |BadRequest RequestToken [RequestToken]
  |EmptyRequest
   deriving Show

-- | Parse a bytestring into a list of tokens and then into a Request. Invalid results (i.e. Left values) are ignored, but are unlikely to be produced anyway. 
parseRequest :: ByteString -> Request
parseRequest = fromRight EmptyRequest . evalParser pRequest "" . fromRight [] . evalParser pRequestToks ""

-- | Parse a list of tokens into a request. Any invalid requests (and thus invalid combinations of RequestTokens) are represented by the BadRequest or EmptyRequest variants.
pRequest :: RequestParser Request
pRequest = try (choice [pGet
                       ,pSet
                       ,pClear
                       ,pRemove
                       ,pAll
                       ,pExit
                       ])
           <|> pBadReq

pGet :: RequestParser Request
pGet = Get <$ satisfy (==TGET)
       <*> pWord
       <* pEnd
       

pSet :: RequestParser Request
pSet = Set <$ satisfy (==TSET)
       <*> pWord
       <*> pWords
       <* pEnd

pClear :: RequestParser Request
pClear = Clear <$ satisfy (==TCLEAR)
         <* pEnd

pRemove :: RequestParser Request
pRemove = Remove <$ satisfy (==TREMOVE)
          <*> pWord
          <* pEnd

pAll :: RequestParser Request
pAll = All <$ satisfy (==TALL)
       <* pEnd

pExit :: RequestParser Request
pExit = Exit <$ satisfy (==TEXIT)
        <* pEnd

pWord :: RequestParser ByteString
pWord = anyToken >>= \case
  TINVALID _ -> empty
  verb -> pure . C.pack . show $ verb

pWords :: RequestParser ByteString
pWords = B.intercalate " " <$> (some pWord)

pEnd :: RequestParser ()
pEnd = endOfInput

pBadReq :: RequestParser Request
pBadReq = (BadRequest <$> anyToken <*> many anyToken) <|> pure EmptyRequest

-- | Produce an error message based on the invalid request.
reportBadReqError :: Request -> ByteString
reportBadReqError (BadRequest _ (filter invalidTok -> x:_)) = C.pack "ERROR invalid characters in request.\n"
reportBadReqError (BadRequest (TINVALID _) _) = C.pack "ERROR invalid characters in request.\n"
reportBadReqError (BadRequest (TWORD _) _) = C.pack "ERROR invalid verb found at start of command.\n"
reportBadReqError (BadRequest TGET args) = C.pack $ "ERROR expected 1 word as argument for GET, " ++ (show . length $ args) ++  " given.\n"
reportBadReqError (BadRequest TREMOVE args) = C.pack $ "ERROR expected 1 word as argument for REMOVE, " ++ (show . length $ args) ++  " given.\n"
reportBadReqError (BadRequest TSET args) = C.pack $ "ERROR expected at least 2 words as arguments for SET, " ++ (show . length $ args) ++  " given.\n"
reportBadReqError (BadRequest TCLEAR args) = C.pack $ "ERROR no arguments expected for CLEAR, " ++ (show . length $ args) ++  " given.\n"
reportBadReqError (BadRequest TALL args) = C.pack $ "ERROR no arguments expected for ALL, " ++ (show . length $ args) ++  " given.\n"
reportBadReqError (BadRequest TEXIT args) = C.pack $ "ERROR no arguments expected for EXIT, " ++ (show . length $ args) ++  " given.\n"

invalidTok :: RequestToken -> Bool
invalidTok (TINVALID _) = True
invalidTok _ = False

instance Stream [a] where
  type Chunk [a] = [a]
  type Token [a] = a
  streamUncons [] = Nothing
  streamUncons (x:xs) = Just (x,xs)

  updatePosToken _ _ (Pos f l c) = Pos f l (c+1)
