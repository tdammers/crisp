module Language.Crisp.Lexer
( lexer
, Lexeme (..)
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

data Lexeme
  = OpenParL
  | CloseParL
  | DotL
  | StringLitL Text
  | IntLitL Integer
  | SymbolL Text
  deriving (Show, Read, Eq, Ord)

lexer :: String -> Either (ParseError Char ()) [Lexeme]
lexer = parse lexemes "<input>"

type Parser = Parsec () String

lexemes :: Parser [Lexeme]
lexemes = many (lexeme <* many skippable)

skippable :: Parser ()
skippable = whitespace <|> comment

whitespace :: Parser ()
whitespace = oneOf " \t\r\n\b," *> pure ()

comment :: Parser ()
comment = do
  char ';'
  manyTill anyChar (char '\n')
  pure ()

lexeme :: Parser Lexeme
lexeme = openPar
       <|> closePar
       <|> dot
       <|> stringLit
       <|> intLit
       <|> symbol
       <?> "valid lexeme"

openPar :: Parser Lexeme
openPar = char '(' *> pure OpenParL

closePar :: Parser Lexeme
closePar = char ')' *> pure CloseParL

dot :: Parser Lexeme
dot = char '.' *> pure DotL

stringLit :: Parser Lexeme
stringLit = fmap (StringLitL . Text.pack) $ char '"' *> many stringChar <* char '"'

stringChar :: Parser Char
stringChar = quotedChar
           <|> noneOf "\"\\"

quotedChar :: Parser Char
quotedChar = char '\\' *>
  choice
    [ char 'n' *> return '\n'
    , char 'r' *> return '\r'
    , anyChar
    ]

intLit :: Parser Lexeme
intLit = IntLitL . read <$> some (satisfy isDigit)

symbol :: Parser Lexeme
symbol =
  fmap (SymbolL . Text.pack) $ some (satisfy (\c -> not (isSpace c) && not (c `elem` "();.")))
