{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE TypeFamilies #-}
module Language.Crisp.Parser
( parser
)
where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Applicative
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Char
import Data.Proxy
import Data.List
import Data.Monoid hiding ((<>))
import Data.Semigroup
import qualified Data.List.NonEmpty as NonEmpty
import Data.Void

import Language.Crisp.Value
import Language.Crisp.Lexer

parser :: [Lexeme] -> Either (ParseError Lexeme Void) Value
parser = parse value "<input>"

instance Stream [Lexeme] where
  type Token [Lexeme] = Lexeme
  type Tokens [Lexeme] = [Lexeme]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  advance1 Proxy = defaultAdvance1
  advanceN Proxy w = foldl' (defaultAdvance1 w)
  take1_ [] = Nothing
  take1_ (t:ts) = Just (t, ts)
  takeN_ n s
    | n <= 0    = Just ([], s)
    | null s    = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span

type Parser = Parsec Void [Lexeme]

value :: Parser Value
value = dottedPairOrSimple

dottedPairOrSimple :: Parser Value
dottedPairOrSimple = do
  lhs <- simpleValue
  dottedPair lhs <|> pure lhs
  where
    dottedPair :: Value -> Parser Value
    dottedPair lhs = do
      exactly DotL
      rhs <- value
      pure $ Cons lhs rhs

simpleValue :: Parser Value
simpleValue =
      list
  <|> scalar

list :: Parser Value
list = do
  exactly OpenParL
  listItems <- manyTill value $ exactly CloseParL
  pure $ listToValue listItems

listToValue :: [Value] -> Value
listToValue (x:xs) = Cons x $ listToValue xs
listToValue _ = Nil

exactly :: Lexeme -> Parser ()
exactly x = satisfy (== x) *> pure ()

scalar :: Parser Value
scalar = do
  anyChar >>= \case
    StringLitL str -> pure $ String str
    IntLitL i -> pure $ Int i
    SymbolL "true" -> pure $ Bool True
    SymbolL "false" -> pure $ Bool False
    SymbolL s -> pure $ Atom s
    x -> failure (Just $ Tokens [x]) [lbl "StringLitL", lbl "IntLitL", lbl "SymbolL"]

lbl :: String -> ErrorItem t
lbl = Label . NonEmpty.fromList


-- Helpers

-- | Default positioning function designed to work with simple streams where
-- tokens do not contain info about their position in the stream. Thus it
-- just returns the given 'SourcePos' without re-positioning.

defaultPositionAt :: SourcePos -> a -> SourcePos
defaultPositionAt pos _ = pos
{-# INLINE defaultPositionAt #-}

-- | Update a source position given a token. The first argument specifies
-- the tab width. If the character is a newline (\'\\n\') the line number is
-- incremented by 1 and column number is reset to 1. If the character is a
-- tab (\'\\t\') the column number is incremented to the nearest tab
-- position. In all other cases, the column is incremented by 1.

defaultAdvance1 :: Pos               -- ^ Tab width
                -> SourcePos         -- ^ Current position
                -> t                 -- ^ Current token
                -> SourcePos         -- ^ Incremented position
defaultAdvance1 width (SourcePos n l c) t = npos
  where
    w  = unPos width
    c' = unPos c
    npos = SourcePos n l (c <> pos1)
{-# INLINE defaultAdvance1 #-}
