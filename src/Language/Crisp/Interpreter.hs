{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE OverloadedLists #-}
{-#LANGUAGE DeriveFunctor #-}
{-#LANGUAGE GeneralizedNewtypeDeriving #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-#LANGUAGE MultiParamTypeClasses #-}
{-#LANGUAGE FunctionalDependencies #-}
{-#LANGUAGE FlexibleInstances #-}
{-#LANGUAGE LambdaCase #-}
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
import Data.Void

class Monad m => MonadRaise m where
  raise :: forall a. Text -> m a

class MonadRaise m => MonadEval n m where
  lookupBuiltin :: Text -> m (Maybe (Value n))
  evalNativeFn :: Scope n -> n -> Value n -> m (Value n)

class Monad m => MonadExec n m where
  execNative :: Scope n -> n -> m (Value n)

exec :: (MonadExec n m, MonadEval n m) => Scope n -> Value n -> m (Value n)
exec scope (Native eff) = execNative scope eff
exec scope v = pure v

eval :: MonadEval n m => Scope n -> Value n -> m (Value n)
eval scope (Atom name) =
  lookupAtom scope name >>= eval scope
eval scope (Cons car cdr) = do
  evalCons scope car cdr
eval _ x = pure x

lookupAtom :: MonadEval n m => Scope n -> Text -> m (Value n)
lookupAtom scope varName = do
  case Map.lookup varName scope of
    Just x -> do
      pure x
    Nothing ->
      lookupBuiltin varName >>= \case
        Just x -> do
          pure x
        Nothing -> do
          pure (Atom varName)

evalCons :: MonadEval n m => Scope n -> Value n -> Value n -> m (Value n)
evalCons scope f args = case f of
  Native nf -> do
    argVals <- consMapM (eval scope) args
    evalNativeFn scope nf argVals
  Lambda argsSpec closure body -> do
    argVals <- consMapM (eval scope) args
    let scope' = mapArgs argsSpec argVals <> closure
    eval scope' body
  Atom "let" ->
    evalLet scope args
  Atom "lambda" ->
    evalLambda scope args
  Atom text -> do
    f' <- eval scope f
    case f' of
      Atom text -> raise $ "Cannot resolve atom " <> text <> " to anything applicable"
      _ -> evalCons scope f' args
  x ->
    raise $ "Cannot apply " <> valToText x <> ", because it is neither a function nor a macro"

evalLet :: MonadEval n m => Scope n -> Value n -> m (Value n)
evalLet scope = \case
  [[Atom varname, expr], body] -> do
    exprVal <- eval scope expr
    let scope' = Map.insert varname exprVal scope
    eval scope' body
  x -> raise $ "Invalid arguments to `let`: " <> valToText x

evalLambda :: MonadEval n m => Scope n -> Value n -> m (Value n)
evalLambda scope = \case
  [argnameAtoms, body] -> do
    argnames <- forM (consToList argnameAtoms) $ \case
      Atom name -> return name
      x -> raise $ "Invalid argument name: " <> valToText x
    -- TODO: (a b & args) syntax
    pure $ Lambda (ArgsSpec argnames Nothing) scope body
  x -> raise $ "Invalid arguments to `lambda`: " <> valToText x

mapArgs :: ArgsSpec -> Value n -> Scope n
mapArgs (ArgsSpec [] Nothing) args =
  Map.empty
mapArgs (ArgsSpec [] (Just remainingName)) args =
  Map.singleton remainingName args
mapArgs (ArgsSpec (pname:pnames) remainingName) (Cons arg args) =
  Map.singleton pname arg <> mapArgs (ArgsSpec pnames remainingName) args
mapArgs (ArgsSpec (pname:pnames) remainingName) x =
  Map.singleton pname x <> mapArgs (ArgsSpec pnames remainingName) Nil

strValue :: Value n -> Text
strValue Nil = ""
strValue (Cons a b) = strValue a <> strValue b
strValue (String str) = str
strValue (Int i) = Text.pack . show $ i
strValue x = valToText $ x
