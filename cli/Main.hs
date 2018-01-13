{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
module Main where

import Language.Crisp
import System.IO
import Control.Monad
import Control.Exception
import qualified Text.Megaparsec as P
import Data.Void
import Data.Bool
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

data LexerError = LexerError (P.ParseError Char Void)
  deriving (Show)
instance Exception LexerError where

data ParserError = ParserError (P.ParseError Lexeme Void)
  deriving (Show)
instance Exception ParserError where

data RuntimeError = RuntimeError Text
  deriving (Show)
instance Exception RuntimeError where

unlessM :: IO Bool -> IO () -> IO ()
unlessM p a =
  p >>= bool a (return ())

main :: IO ()
main = do
  hPutStr stderr "λ> "
  hFlush stderr
  unlessM isEOF $ do
    input <- getLine
    when (not . null $ input) $ flip catches handles $ do
      lexemes <- either (throw . LexerError) return $ lexer input
      parsed <- either (throw . ParserError) return $ parser lexemes
      result <- either (throw . RuntimeError) return $ pureEval_ parsed []
      putStrLn $ valToString result
    main
  where
    handles =
      [ Handler (\(LexerError e) -> hPutStrLn stderr . P.parseErrorPretty $ e)
      , Handler (\(ParserError e) -> hPutStrLn stderr . P.parseErrorPretty $ e)
      , Handler (\(RuntimeError msg) -> Text.hPutStrLn stderr msg)
      , Handler (\(e :: SomeException) -> hPutStrLn stderr . show $ e)
      ]
