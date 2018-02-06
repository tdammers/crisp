{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE LambdaCase #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE MultiParamTypeClasses #-}
module Main where

import Language.Crisp
import System.IO
import System.Environment
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

data Eff
  = NOP
  | Print Text
  | Read
  | EffectError Text

unlessM :: IO Bool -> IO () -> IO ()
unlessM p a =
  p >>= bool a (return ())

data MainOpts =
  MainOpts
    { inputFiles :: [FilePath]
    , interactive :: Bool
    , dumpLex :: Bool
    , dumpAST :: Bool
    }
    deriving (Show)

defOpts :: MainOpts
defOpts =
  MainOpts
    { inputFiles = []
    , interactive = False
    , dumpLex = False
    , dumpAST = False
    }

parseOpts :: [String] -> Either String MainOpts
parseOpts args =
  case go defOpts args of
    (_, x:xs) -> Left $ "Unknown option " ++ x
    (opts, _) -> Right opts
  where
    go :: MainOpts -> [String] -> (MainOpts, [String])
    go opts [] = (opts, [])
    go opts args@(('-':flag):xs) =
      case flag of
        "i" -> go opts { interactive = True } xs
        "-dump-lex" -> go opts { dumpLex = True } xs
        "-dump-ast" -> go opts { dumpAST = True } xs
        _ -> (opts, args)
    go opts args@(filename:xs) =
      go opts { inputFiles = inputFiles opts ++ [filename] } xs

main :: IO ()
main = do
  fmap parseOpts getArgs >>= \case
    Left err -> error err
    Right opts -> run opts

run :: MainOpts -> IO ()
run opts = do
  case (interactive opts, inputFiles opts) of
    (True, []) ->
      runInteractive opts
    (False, []) -> do
      hIsTerminalDevice stdin >>= \case
        True ->
          runInteractive opts
        False ->
          runStdin opts
    (_, xs) ->
      mapM_ (runFile opts) xs

runInteractive :: MainOpts -> IO ()
runInteractive opts = do
  hPutStr stderr "λ> "
  hFlush stderr
  unlessM isEOF $ do
    getLine >>= runExpr opts
    runInteractive opts

runFile :: MainOpts -> FilePath -> IO ()
runFile opts fn = do
  readFile fn >>= runExpr opts

runStdin :: MainOpts -> IO ()
runStdin opts = do
  getContents >>= runExpr opts

effectsContext :: Scope Eff
effectsContext =
  [ ("getln", Effect (Atomic Read))
  , ("print", NativeFunction "print" $ const print)
  ]
  where
    print :: Value Eff -> Value Eff
    print = \case
      Nil ->
        Effect (Pure Nil)
      Cons car cdr ->
        let current = Atomic . Print . valToText $ car
            rest = print cdr
        in Effect $ Bind current rest
      x ->
        Effect (Atomic $ EffectError "Invalid arguments to 'print'")

instance MonadExec Eff IO where
  execEff Print v = putStrLn (valToString v) >> pure Nil
  execEff Read _ = String . Text.pack <$> getLine

runExpr :: MainOpts -> String -> IO ()
runExpr opts input =
  when (not . null $ input) $ flip catches handles $ do
    lexemes <- either (throw . LexerError) return $ lexer input
    when (dumpLex opts) (print lexemes)
    parsed <- either (throw . ParserError) return $ parser lexemes
    when (dumpAST opts) (putStrLn $ valToString parsed)
    result :: Value Eff <- either (throw . RuntimeError) return =<< executeM_ parsed effectsContext
    putStrLn $ valToString result
    where
      handles =
        [ Handler (\(LexerError e) -> hPutStrLn stderr . P.parseErrorPretty $ e)
        , Handler (\(ParserError e) -> hPutStrLn stderr . P.parseErrorPretty $ e)
        , Handler (\(RuntimeError msg) -> Text.hPutStrLn stderr msg)
        , Handler (\(e :: SomeException) -> hPutStrLn stderr . show $ e)
        ]
