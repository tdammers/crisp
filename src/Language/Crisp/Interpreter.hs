{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
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
import Control.Monad.Reader
import Control.Monad.Except
import Data.Maybe

import Language.Crisp.Value

eval :: (Monad m, MonadEval m) => Value -> m Value
eval (Cons x args) = do
  f <- eval x
  argVals <- consMapM eval args
  case f of
    Lambda argsSpec body ->
      apply argsSpec argVals body
    Atom a -> do
      -- This is a special case: the 'Value' type cannot represent builtins,
      -- this keeps things considerably simpler, and avoids having to make
      -- 'Value' polymorphic over the execution context.
      b <- getBuiltin a
      case b of
        Nothing ->
          raise $ "Builtin not found: " <> a
        Just builtin ->
          builtin argVals
    x ->
      raise $ "Not a function: " <> valToText x
eval (Atom a) = fromMaybe (Atom a) <$> getVar a
eval x = pure x

evalArgs :: (Monad m, MonadEval m) => Value -> m [Value]
evalArgs Nil = pure []
evalArgs (Cons x xs) = (:) <$> eval x <*> evalArgs xs
evalArgs x =
  raise $ "Not a well-formed argument list: " <> valToText x

apply :: (Monad m, MonadEval m) => ArgsSpec -> Value -> Value -> m Value
apply argsSpec args body =
  raise $ "Cannot evaluate functions yet"

class MonadEval m where
  setVar :: Text -> Value -> m ()
  getVar :: Text -> m (Maybe Value)
  getBuiltin :: Text -> m (Maybe (Value -> m Value))
  raise :: Text -> m a

newtype PureEval a =
  PureEval { unPureEval :: ExceptT Text (ReaderT (Map Text (Value -> PureEval Value)) (State (Map Text Value))) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (Map Text Value)
    , MonadError Text
    , MonadReader (Map Text (Value -> PureEval Value))
    )

instance MonadEval PureEval where
  setVar n v = modify $ Map.insert n v
  getVar n = Map.lookup n <$> get
  getBuiltin n = Map.lookup n <$> ask
  raise msg = throwError msg

pureBuiltins :: (MonadEval m, Monad m) => Map Text (Value -> m Value)
pureBuiltins =
  [ ("+", sumValues)
  , ("list", listValues)
  , ("eval", evalValues)
  ]

runPureEval :: PureEval a -> Map Text Value -> (Either Text a, Map Text Value)
runPureEval = runState . flip runReaderT pureBuiltins . runExceptT . unPureEval

pureEval :: Value -> Map Text Value -> (Either Text Value, Map Text Value)
pureEval value context = runPureEval (eval value) context

pureEval_ :: Value -> Map Text Value -> Either Text Value
pureEval_ value context = fst $ pureEval value context

sumValues :: (Monad m, MonadEval m) => Value -> m Value
sumValues = fmap Int . intSumValues
  where
    intSumValues Nil = pure 0
    intSumValues (Cons (Int i) xs) = (i +) <$> intSumValues xs
    intSumValues x = raise $ "Cannot perform addition on " <> valToText x

listValues :: (Monad m, MonadEval m) => Value -> m Value
listValues = pure

evalValues :: (Monad m, MonadEval m) => Value -> m Value
evalValues args = consMapM eval args
