{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad.Identity
import Data.Maybe
import Control.Applicative
import Debug.Trace

import Language.Crisp.Value

eval :: (Monad m, MonadEval m) => Value -> m Value
eval = eval'
-- eval val =
--   traceM $ "<- " <> valToString val
--   val' <- eval' val
--   traceM $ valToString val <> " -> " <> valToString val'
--   return val'

eval' :: (Monad m, MonadEval m) => Value -> m Value
-- Special form: (quote x)
eval' (Cons (Atom "quote") (Cons arg Nil)) =
  pure arg
eval' (Cons (Atom "quote") xs) =
  raise $ "Invalid arguments to (quote): " <> valToText xs
-- Special form: (let (name expr) ... body)
eval' (Cons (Atom "let") args) =
  evalLet args
-- Special form: (lambda (argname0 argname1 ...) body)
eval' (Cons (Atom "lambda") args) =
  evalLambda args
-- S-expressions evaluate to function calls
eval' (Cons x args) = do
  f <- eval x
  argVals <- consMapM eval args
  case f of
    Lambda argsSpec closure body ->
      apply argsSpec argVals closure body
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
-- Atoms evaluate as variable lookups
eval' (Atom a) = fromMaybe (Atom a) <$> getVar a
-- Anything else evaluates to itself
eval' x = pure x

evalLet :: (Monad m, MonadEval m) => Value -> m Value
evalLet Nil =
  pure Nil
evalLet (Cons expr Nil) =
  eval expr
evalLet (Cons binding remainder) =
  case binding of
    Cons (Atom name) (Cons valExpr Nil) -> do
      val <- eval valExpr
      addBinding name val $ evalLet remainder
    x ->
      raise $ "Malformed let bindingL " <> valToText x

evalLambda :: (Monad m, MonadEval m) => Value -> m Value
evalLambda (Cons argnames (Cons bodyExpr Nil)) = do
  Lambda <$> extractArgsSpec argnames
         <*> getBindings
         <*> pure bodyExpr

extractArgsSpec :: (Monad m, MonadEval m) => Value -> m ArgsSpec
extractArgsSpec Nil =
  pure $ ArgsSpec [] Nothing
extractArgsSpec (Cons (Atom "&") (Atom n)) =
  pure $ ArgsSpec [] (Just n)
extractArgsSpec (Cons (Atom "&") x) =
  raise $ "Invalid argument specification in lambda form: " <> valToText x <> " (exactly one atom required after &)"
extractArgsSpec (Cons (Atom n) xs) = do
  spec <- extractArgsSpec xs
  pure spec { positionalArgNames = n : positionalArgNames spec }
extractArgsSpec x =
  raise $ "Invalid argument specification in lambda form: " <> valToText x

evalArgs :: (Monad m, MonadEval m) => Value -> m [Value]
evalArgs Nil = pure []
evalArgs (Cons x xs) = (:) <$> eval x <*> evalArgs xs
evalArgs x =
  raise $ "Not a well-formed argument list: " <> valToText x

apply :: forall m. (Monad m, MonadEval m) => ArgsSpec -> Value -> Scope -> Value -> m Value
apply (ArgsSpec positional remaining) args closure body =
  withBindings closure . bindArgs positional remaining args $ eval body
  where
    bindArgs :: [Text] -> Maybe Text -> Value -> m Value -> m Value
    bindArgs [] Nothing Nil action =
      -- Nothing left to bind, run the action as-is
      action
    bindArgs [] Nothing x action =
      raise $ "Excessive arguments passed to function: " <> valToText x
    bindArgs [] (Just name) x action =
      addBinding name x action
    bindArgs (n:ns) r (Cons v vs) action =
      addBinding n v $ bindArgs ns r vs action
    bindArgs (n:ns) r Nil action =
      raise $ "Missing argument: " <> n
    bindArgs _ _ x action =
      raise $ "Invalid argument structure in function call: " <> valToText x

class MonadEval m where
  setDynVar :: Text -> Value -> m ()
  getDynVar :: Text -> m (Maybe Value)
  withBindings :: Scope -> m a -> m a
  getBindings :: m Scope
  getBuiltin :: Text -> m (Maybe (Value -> m Value))
  raise :: Text -> m a

getBinding :: (Monad m, MonadEval m) => Text -> m (Maybe Value)
getBinding n = Map.lookup n <$> getBindings

addBindings :: (Monad m, MonadEval m) => Scope -> m a -> m a
addBindings additionalBindings a = do
  oldBindings <- getBindings
  let newBindings = Map.union additionalBindings oldBindings
  withBindings newBindings a

addBinding :: (Monad m, MonadEval m) => Text -> Value -> m a -> m a
addBinding n v = addBindings $ Map.singleton n v

getVar :: (Monad m, MonadEval m) => Text -> m (Maybe Value)
getVar n = do
  getBinding n >>= maybe (getDynVar n) (pure . Just)

newtype EvalT m a =
  EvalT { unEvalT :: ExceptT Text (ReaderT Scope (StateT Scope m)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Scope
    , MonadError Text
    , MonadReader Scope
    , MonadIO
    )

instance Monad m => MonadEval (EvalT m) where
  setDynVar n v = modify $ Map.insert n v
  getDynVar n = Map.lookup n <$> get
  getBuiltin n = pure . Map.lookup n $ builtinFunctions
  getBindings = ask
  withBindings = local . const
  raise msg = throwError msg

builtinFunctions :: (MonadEval m, Monad m) => Map Text (Value -> m Value)
builtinFunctions =
  [ ("+", sumValues)
  , ("list", listValues)
  , ("eval", evalValues)
  , ("cons", consValues)
  , ("str", strValues)
  ]

runEvalT :: EvalT m a -> Map Text Value -> m (Either Text a, Map Text Value)
runEvalT = runStateT . flip runReaderT [] . runExceptT . unEvalT

evaluate :: Value -> Map Text Value -> (Either Text Value, Map Text Value)
evaluate value context = runIdentity $ runEvalT (eval value) context

evaluate_ :: Value -> Map Text Value -> Either Text Value
evaluate_ value context = fst $ evaluate value context

sumValues :: (Monad m, MonadEval m) => Value -> m Value
sumValues = fmap Int . intSumValues
  where
    intSumValues Nil = pure 0
    intSumValues (Cons (Int i) xs) = (i +) <$> intSumValues xs
    intSumValues x = raise $ "Cannot perform addition on " <> valToText x

listValues :: (Monad m) => Value -> m Value
listValues = pure

evalValues :: (Monad m, MonadEval m) => Value -> m Value
evalValues args = consMapM eval args

consValues :: (Monad m, MonadEval m) => Value -> m Value
consValues (Cons a (Cons b Nil)) = pure $ Cons a b
consValues x = raise $ "Invalid arguments to cons: " <> valToText x

strValues :: (Monad m) => Value -> m Value
strValues = pure . String . strValue

strValue :: Value -> Text
strValue Nil = ""
strValue (Cons a b) = strValue a <> strValue b
strValue (String str) = str
strValue (Int i) = Text.pack . show $ i
strValue x = valToText $ x
