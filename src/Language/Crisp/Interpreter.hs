{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE FlexibleInstances #-}
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
import Control.Monad.Trans
import Data.Maybe
import Control.Applicative
import Debug.Trace

import Language.Crisp.Value

eval :: (Monad m, MonadEval eff m) => Value eff -> m (Value eff)
eval = eval'

eval' :: (Monad m, MonadEval eff m) => Value eff -> m (Value eff)
-- Special form: ($ macro & args)
eval' (Cons (Atom "$") (Cons macroExpr args)) = do
  macro <- eval macroExpr
  evalAppl macro args >>= eval
eval' (Cons (Atom "$") x) =
  raise $ "Invalid macro application form: " <> valToText x
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
-- Special form: (apply fn arglist), expands to (fn arg0 arg1 ... argN)
eval' (Cons (Atom "apply") args) =
  case args of
    (Cons f (Cons arglistExpr Nil)) -> do
      arglist <- eval arglistExpr
      eval (Cons f arglist)
    (Cons f Nil) ->
      eval (Cons f Nil)
    x ->
      raise $ "Invalid arguments to (apply): " <> valToText x
-- Special form: (cond (condition-1 body-1) ... (condition-n body-n))
eval' (Cons (Atom "cond") conditions) =
  evalCond conditions
-- S-expressions evaluate to function calls
eval' (Cons x args) = do
  f <- eval x
  argVals <- consMapM eval args
  evalAppl f argVals
-- Atoms evaluate as variable lookups
eval' (Atom a) = fromMaybe (Atom a) <$> getVar a
-- Anything else evaluates to itself
eval' x = pure x

evalAppl :: (Monad m, MonadEval eff m) => Value eff -> Value eff -> m (Value eff)
evalAppl f argVals =
  case f of
    Lambda argsSpec closure body ->
      apply argsSpec argVals closure body
    Atom a -> do
      -- This is a special case: the 'Value eff' type cannot represent builtins,
      -- this keeps things considerably simpler, and avoids having to make
      -- 'Value eff' polymorphic over the execution context.
      b <- getBuiltin a
      case b of
        Nothing ->
          raise $ "Builtin not found: " <> a
        Just builtin ->
          builtin argVals
    x ->
      raise $ "Not a function: " <> valToText x

evalLet :: (Monad m, MonadEval eff m) => Value eff -> m (Value eff)
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

evalLambda :: (Monad m, MonadEval eff m) => Value eff -> m (Value eff)
evalLambda (Cons argnames (Cons bodyExpr Nil)) = do
  Lambda <$> extractArgsSpec argnames
         <*> getBindings
         <*> pure bodyExpr

extractArgsSpec :: (Monad m, MonadEval eff m) => Value eff -> m ArgsSpec
extractArgsSpec Nil =
  pure $ ArgsSpec [] Nothing
extractArgsSpec (Cons (Atom "&") (Cons (Atom n) Nil)) =
  pure $ ArgsSpec [] (Just n)
extractArgsSpec (Cons (Atom "&") x) =
  raise $ "Invalid argument specification in lambda form: " <> valToText x <> " (exactly one atom required after &)"
extractArgsSpec (Cons (Atom n) xs) = do
  spec <- extractArgsSpec xs
  pure spec { positionalArgNames = n : positionalArgNames spec }
extractArgsSpec x =
  raise $ "Invalid argument specification in lambda form: " <> valToText x

evalArgs :: (Monad m, MonadEval eff m) => Value eff -> m [Value eff]
evalArgs Nil = pure []
evalArgs (Cons x xs) = (:) <$> eval x <*> evalArgs xs
evalArgs x =
  raise $ "Not a well-formed argument list: " <> valToText x

evalCond :: (Monad m, MonadEval eff m) => Value eff -> m (Value eff)
evalCond Nil = pure Nil
evalCond (Cons (Cons condExpr (Cons bodyExpr Nil)) branches) = do
  condVal <- eval condExpr
  if isTruthy condVal then
    eval bodyExpr
  else
    evalCond branches
evalCond x =
  raise $ "Invalid condition structure in (cond): " <> valToText x

apply :: forall m eff. (Monad m, MonadEval eff m)
      => ArgsSpec -- ^ function argument specification
      -> Value eff -- ^ argument values passed to the fn (evaluated)
      -> Scope eff -- ^ fn's closure (evaluated)
      -> Value eff -- ^ fn body (unevaluated)
      -> m (Value eff)
apply (ArgsSpec positional remaining) args closure body =
  withBindings closure . bindArgs positional remaining args $ eval body
  where
    bindArgs :: [Text] -> Maybe Text -> Value eff -> m (Value eff) -> m (Value eff)
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

class MonadEval eff m | m -> eff where
  setDynVar :: Text -> Value eff -> m ()
  getDynVar :: Text -> m (Maybe (Value eff))
  withBindings :: Scope eff -> m a -> m a
  getBindings :: m (Scope eff)
  getBuiltin :: Text -> m (Maybe (Value eff -> m (Value eff)))
  raise :: Text -> m a

class MonadExec eff m where
  execEff :: eff -> m (Value eff)

execEffect :: (Monad m, MonadEval eff m, MonadExec eff m)
           => Effect eff
           -> Value eff
           -> m (Value eff)
execEffect (Atomic e) val =
  execEff e val
execEffect (Pure v) _ =
  pure v
execEffect (Bind e f) val = do
  lhs <- execEffect e val
  execValue f lhs

execValue :: (Monad m, MonadEval eff m, MonadExec eff m) => Value eff -> Value eff -> m (Value eff)
execValue (Effect e) val =
  execEffect e val
execValue (Lambda argspec closure body) val = do
  e <- apply argspec val closure body
  execValue e Nil
execValue v _ =
  pure v

getBinding :: (Monad m, MonadEval eff m) => Text -> m (Maybe (Value eff))
getBinding n = Map.lookup n <$> getBindings

addBindings :: (Monad m, MonadEval eff m) => Scope eff
                                      -> m a
                                      -> m a
addBindings additionalBindings a = do
  oldBindings <- getBindings
  let newBindings = Map.union additionalBindings oldBindings
  withBindings newBindings a

addBinding :: (Monad m, MonadEval eff m) => Text -> Value eff -> m a -> m a
addBinding n v = addBindings $ Map.singleton n v

getVar :: (Monad m, MonadEval eff m) => Text -> m (Maybe (Value eff))
getVar n = do
  getBinding n >>= maybe (getDynVar n) (pure . Just)

newtype EvalT m eff a =
  EvalT { unEvalT :: ExceptT Text (ReaderT (Scope eff) (StateT (Scope eff) m)) a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState (Scope eff)
    , MonadError Text
    , MonadReader (Scope eff)
    , MonadIO
    )

instance Monad m => MonadEval eff (EvalT m eff) where
  setDynVar n v = modify $ Map.insert n v
  getDynVar n = Map.lookup n <$> get
  getBuiltin n = pure . Map.lookup n $ builtinFunctions
  getBindings = ask
  withBindings = local . const
  raise msg = throwError msg

instance (Monad m, MonadExec eff m) => MonadExec eff (EvalT m eff) where
  execEff eff val = liftEvalT $ execEff eff val

builtinFunctions :: (MonadEval eff m, Monad m) => Map Text (Value eff -> m (Value eff))
builtinFunctions =
  [ ("+", sumValues)
  , ("list", listValues)
  , ("eval", evalValues)
  , ("cons", consValues)
  , ("str", strValues)
  , ("bind", bindValues)
  , ("pure", pureValues)
  ]

runEvalT :: EvalT m eff a
         -> Scope eff
         -> m (Either Text a, Scope eff)
runEvalT = runStateT . flip runReaderT [] . runExceptT . unEvalT

liftEvalT :: Monad m
          => m a
          -> EvalT m eff a
liftEvalT = EvalT . lift . lift . lift

executeM :: (Monad m, MonadExec eff m)
         => Value eff
         -> Scope eff
         -> m (Either Text (Value eff), Scope eff)
executeM value context =
  runEvalT (eval value >>= flip execValue Nil) context

executeM_ :: (Monad m, MonadExec eff m)
           => Value eff
           -> Scope eff
           -> m (Either Text (Value eff))
executeM_ value context = fst <$> executeM value context


evaluateM :: Monad m
          => Value eff
          -> Scope eff
          -> m (Either Text (Value eff), Scope eff)
evaluateM value context =
  runEvalT (eval value) context

evaluateM_ :: Monad m
           => Value eff
           -> Scope eff
           -> m (Either Text (Value eff))
evaluateM_ value context = fst <$> evaluateM value context

evaluate :: Value eff
         -> Scope eff
         -> (Either Text (Value eff), Scope eff)
evaluate value context =
  runIdentity $ runEvalT (eval value) context

evaluate_ :: Value eff
          -> Scope eff
          -> Either Text (Value eff)
evaluate_ value context = fst $ evaluate value context

sumValues :: (Monad m, MonadEval eff m) => Value eff -> m (Value eff)
sumValues = fmap Int . intSumValues
  where
    intSumValues Nil = pure 0
    intSumValues (Cons (Int i) xs) = (i +) <$> intSumValues xs
    intSumValues x = raise $ "Cannot perform addition on " <> valToText x

listValues :: (Monad m) => Value eff -> m (Value eff)
listValues = pure

evalValues :: (Monad m, MonadEval eff m) => Value eff -> m (Value eff)
evalValues args = consMapM eval args

consValues :: (Monad m, MonadEval eff m) => Value eff -> m (Value eff)
consValues (Cons a (Cons b Nil)) = pure $ Cons a b
consValues x = raise $ "Invalid arguments to cons: " <> valToText x

strValues :: (Monad m) => Value eff -> m (Value eff)
strValues = pure . String . strValue

strValue :: Value eff -> Text
strValue Nil = ""
strValue (Cons a b) = strValue a <> strValue b
strValue (String str) = str
strValue (Int i) = Text.pack . show $ i
strValue x = valToText $ x

bindEff :: (Monad m, MonadEval eff m) => Value eff -> m (Effect eff)
bindEff (Cons x y) = case x of
  Effect e ->
    case y of
      Nil -> pure e
      rhs -> Bind e <$> bindValues rhs
  v -> bindEff (Cons (Effect (Pure v)) y)
bindEff x = raise $ "Cannot bind value that is not an effect: " <> valToText x

bindValues :: (Monad m, MonadEval eff m) => Value eff -> m (Value eff)
bindValues v = Effect <$> bindEff v

pureValues :: (Monad m, MonadEval eff m) => Value eff -> m (Value eff)
pureValues = pure . Effect . Pure
