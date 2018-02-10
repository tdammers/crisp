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
import Data.Monoid

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
  = NoE
  | PrintE Text
  | ReadE
  | ErrorE Text
  deriving (Show, Eq)

data Builtin
  = IdentityF
  | PrintF
  | BindF
  deriving (Show, Eq)

data Native
  = NativeEff Eff
  | NativeFunc Builtin
  | NativeBinding Native (Value Native)
  deriving (Show, Eq)

instance MonadRaise IO where
  raise msg = throw $ RuntimeError msg

instance MonadEval Native IO where
  lookupBuiltin = \case
    "identity" -> pure . Just . Native $ NativeFunc IdentityF
    "print" -> pure . Just . Native $ NativeFunc PrintF
    ">>=" -> pure . Just . Native $ NativeFunc BindF
    "read" -> pure . Just . Native $ NativeEff ReadE
    x -> pure Nothing

  evalNativeFn scope nf args = case nf of
    NativeFunc IdentityF ->
      case args of
        Cons car _ -> pure car
        _ -> pure Nil

    NativeFunc PrintF -> do
      let str = Text.unlines . map valToText . consToList $ args
      pure $ Native (NativeEff (PrintE str))

    NativeFunc BindF -> do
      case args of
        Cons (Native a) (Cons rhs Nil) ->
          pure . Native $ NativeBinding a rhs
        x ->
          raise $ "Invalid arguments to >>=: " <> Text.pack (show x)

instance MonadExec Native IO where
  execNative scope = \case
    NativeFunc f ->
      raise $ Text.pack (show f) <> " is not an effect, but a function"
    NativeEff e ->
      execEff e
    NativeBinding e f -> do
      v <- execNative scope e
      exec scope =<< eval scope (Cons f (Cons v Nil))

execEff :: Eff -> IO (Value Native)
execEff NoE = pure Nil
execEff (PrintE str) = Text.putStrLn str >> pure Nil
execEff ReadE = String <$> Text.getLine
execEff (ErrorE err) = raise err

unlessM :: IO Bool -> IO () -> IO ()
unlessM p a =
  p >>= bool a (return ())

data MainOpts =
  MainOpts
    { inputFiles :: [FilePath]
    , interactive :: Bool
    , dumpLex :: Bool
    , dumpAST :: Bool
    , dumpShow :: Bool
    }
    deriving (Show)

defOpts :: MainOpts
defOpts =
  MainOpts
    { inputFiles = []
    , interactive = False
    , dumpLex = False
    , dumpAST = False
    , dumpShow = False
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
        "-dump-show" -> go opts { dumpShow = True } xs
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

runExpr :: MainOpts -> String -> IO ()
runExpr opts input =
  when (not . null $ input) $ flip catches handles $ do
    lexemes <- either (throw . LexerError) return $ lexer input
    when (dumpLex opts) (print lexemes)
    parsed <- either (throw . ParserError) return $ parser lexemes
    when (dumpAST opts) $
      if (dumpShow opts) then
        (print parsed)
      else
        (putStrLn $ valToString parsed)
    result :: Value Native <- exec mempty =<< eval mempty parsed
    when (dumpShow opts) (print result)
    putStrLn $ valToString result
    where
      handles =
        [ Handler (\(LexerError e) -> hPutStrLn stderr . P.parseErrorPretty $ e)
        , Handler (\(ParserError e) -> hPutStrLn stderr . P.parseErrorPretty $ e)
        , Handler (\(RuntimeError msg) -> Text.hPutStrLn stderr msg)
        , Handler (\(e :: SomeException) -> hPutStrLn stderr . show $ e)
        ]
