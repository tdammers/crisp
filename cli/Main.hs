{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE LambdaCase #-}
module Main where

import Language.Crisp
import System.IO
import Control.Monad
import Control.Exception

main :: IO ()
main = forever $ do
  hPutStr stderr "λ> "
  hFlush stderr
  input <- getLine
  when (not . null $ input) $ flip catch handle $ do
    lexemes <- either (error . show) return $ lexer input
    parsed <- either (error . show) return $ parser lexemes
    result <- either (error . show) return $ pureEval_ parsed []
    print parsed
  where
    handle :: SomeException -> IO ()
    handle = hPutStrLn stderr . show
