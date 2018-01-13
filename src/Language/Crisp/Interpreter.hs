{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Crisp.Interpreter
where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text (Text)
import Data.Text as Text
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Semigroup
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe

import Language.Crisp.Value

class MonadEval m where
  setVar :: Text -> Value -> m ()
  getVar :: Text -> m Value
  raise :: Text -> m a

eval :: (Monad m, MonadEval m) => Value -> m Value
eval (Cons x args) = do
  f <- eval x
  case f of
    Lambda argsSpec body ->
      apply argsSpec args body
    x ->
      raise $ "Not a function: " <> Text.pack (valToString x)
eval x = pure x

evalArgs :: (Monad m, MonadEval m) => Value -> m [Value]
evalArgs Nil = pure []
evalArgs (Cons x xs) = (:) <$> eval x <*> evalArgs xs
evalArgs x =
  raise $ "Not a well-formed argument list: " <> Text.pack (valToString x)

apply :: (Monad m, MonadEval m) => ArgsSpec -> Value -> Value -> m Value
apply argsSpec args body =
  raise $ "Cannot evaluate functions yet"

newtype PureEval a =
  PureEval { unPureEval :: ExceptT Text (State (Map Text Value)) a }
  deriving (Functor, Applicative, Monad, MonadState (Map Text Value), MonadError Text)

instance MonadEval PureEval where
  setVar n v = modify $ Map.insert n v
  getVar n = fromMaybe Nil . Map.lookup n <$> get
  raise msg = throwError msg

runPureEval :: PureEval a -> Map Text Value -> (Either Text a, Map Text Value)
runPureEval = runState . runExceptT . unPureEval

pureEval :: Value -> Map Text Value -> (Either Text Value, Map Text Value)
pureEval value context = runPureEval (eval value) context

pureEval_ :: Value -> Map Text Value -> Either Text Value
pureEval_ value context = fst $ pureEval value context
