module Main where

import Language.Crisp

main :: IO ()
main = do
  input <- getContents
  lexemes <- either (error . show) return $ lexer input
  parsed <- either (error . show) return $ parser lexemes
  print parsed
