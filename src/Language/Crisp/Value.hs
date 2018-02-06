module Language.Crisp.Value
where

import qualified Data.Text (Text)
import Data.Text as Text
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Map (Map)

type Scope eff = Map Text (Value eff)

-- | Values we can represent
data Value eff
  = Nil -- ^ the empty list, doubling as the "null" value
  | Cons (Value eff) (Value eff) -- ^ a cons cell (Cons car cdr)
  | Bool Bool -- ^ a boolean (true or false)
  | Int Integer -- ^ integer number
  | String Text -- ^ unicode string
  | Bytes ByteString -- ^ byte array
  | Atom Text -- ^ a symbol; typically used to dereference variables
  | Lambda ArgsSpec (Scope eff) (Value eff) -- ^ lambda function: Lambda args closure body
  | NativeFunction Text (Scope eff -> Value eff -> Value eff) -- ^ pure builtin function
  | Effect (Effect eff)

data Effect eff
  = Pure (Value eff)
  | Atomic eff
  | Bind (Effect eff) (Value eff)

consMap :: (Value eff -> Value eff) -> Value eff -> Value eff
consMap f (Cons a b) = Cons (f a) (consMap f b)
consMap f x = f x

consFor :: Value eff -> (Value eff -> Value eff) -> Value eff
consFor = flip consMap

consMapM :: Monad m => (Value eff -> m (Value eff)) -> Value eff -> m (Value eff)
consMapM f (Cons a b) = Cons <$> f a <*> consMapM f b
consMapM f x = f x

consForM :: Monad m => Value eff -> (Value eff -> m (Value eff)) -> m (Value eff)
consForM = flip consMapM

isTruthy :: Value eff -> Bool
isTruthy Nil = False
isTruthy (Bool False) = False
isTruthy _ = True

isFalsy :: Value eff -> Bool
isFalsy = not . isTruthy

isNil :: Value eff -> Bool
isNil Nil = True
isNil _ = False

isCons :: Value eff -> Bool
isCons (Cons _ _) = True
isCons _ = False

data ArgsSpec
  = ArgsSpec
      { positionalArgNames :: [Text]
      , remainingArgsName :: Maybe Text
      }
      deriving (Show, Read, Eq)

valToString :: Value eff -> String
valToString Nil = "()"
valToString (Cons a Nil) = "(" ++ valToString a ++ printConsTail Nil
valToString (Cons a (Cons b c)) = "(" ++ valToString a ++ printConsTail (Cons b c)
valToString (Cons a b) = valToString a ++ " . " ++ valToString b
valToString (Bool True) = "true"
valToString (Bool False) = "false"
valToString (String t) = show t
valToString (Bytes b) = show b
valToString (Int i) = show i
valToString (Atom a) = Text.unpack a
valToString (Lambda args closure body) = "<<function>>"
valToString (Effect _) = "<<effect>>"

valToText :: Value eff -> Text
valToText = Text.pack . valToString

printConsTail :: Value eff -> String
printConsTail Nil = ")"
printConsTail (Cons a b) = " " ++ valToString a ++ printConsTail b
printConsTail x = " . " ++ valToString x ++ ")"
