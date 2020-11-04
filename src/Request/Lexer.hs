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
  |TKEY ByteString
  |TEOL deriving (Eq,Show)

pRequestToks :: RequestLexer [RequestToken]
pRequestToks = removeEmptyLines <$> some (try pKeyTok <|> try pWordTok <|> pEOLTok)

removeEmptyLines :: [RequestToken] -> [RequestToken]
removeEmptyLines = unlines . filter (not . null) . lines
  where
    lines [] = []
    lines xs = let (line,rest) = span (/= TEOL) xs in line : (lines (drop 1 rest))
    unlines [] = []
    unlines (x:[]) = x
    unlines (x:xs) = x <> (TEOL : unlines xs)

pKeyTok :: RequestLexer RequestToken
pKeyTok = many pSpace *>
  (TKEY <$> tokenWhile1 ((not . flip elem [' ','\r','\n',':']) . chr . fromIntegral))
  <* char (fromIntegral . ord $ ':')
  <* (try (void $ some pSpace))

pWordTok :: RequestLexer RequestToken
pWordTok = many pSpace
  *> (TWORD <$> tokenWhile1 ((not . flip elem [' ','\r','\n']) . chr . fromIntegral))
  <* (try (void $ some pSpace) <|> try (void (lookahead pEOLTok)) <|> endOfInput)

pEOLTok :: RequestLexer RequestToken
pEOLTok = TEOL <$ (optional (string "\r") *> (string "\n"))

pSpace :: RequestLexer ()
pSpace = void $ char (fromIntegral . ord $ ' ')
