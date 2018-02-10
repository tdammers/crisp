module Language.Crisp.Value
where

import qualified Data.Text (Text)
import Data.Text as Text
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Map (Map)

type Scope n = Map Text (Value n)

-- | Values we can represent
data Value n
  = Nil -- ^ the empty list, doubling as the "null" value
  | Cons (Value n) (Value n) -- ^ a cons cell (Cons car cdr)
  | Bool Bool -- ^ a boolean (true or false)
  | Int Integer -- ^ integer number
  | String Text -- ^ unicode string
  | Bytes ByteString -- ^ byte array
  | Atom Text -- ^ a symbol; typically used to dereference variables
  | Lambda ArgsSpec (Scope n) (Value n) -- ^ lambda function: Lambda args closure body
  | Native n -- ^ a native value: this could be a function, an effect, etc.
  deriving (Show, Eq)

consMap :: (Value n -> Value n) -> Value n -> Value n
consMap f (Cons a b) = Cons (f a) (consMap f b)
consMap f x = f x

consFor :: Value n -> (Value n -> Value n) -> Value n
consFor = flip consMap

consMapM :: Applicative m => (Value n -> m (Value n)) -> Value n -> m (Value n)
consMapM f (Cons a b) = Cons <$> f a <*> consMapM f b
consMapM f x = f x

consForM :: Applicative m => Value n -> (Value n -> m (Value n)) -> m (Value n)
consForM = flip consMapM

consToList :: Value n -> [Value n]
consToList Nil = []
consToList (Cons a b) = a:consToList b
consToList x = error $ "Cannot make list from " ++ valToString x

isTruthy :: Value n -> Bool
isTruthy Nil = False
isTruthy (Bool False) = False
isTruthy _ = True

isFalsy :: Value n -> Bool
isFalsy = not . isTruthy

isNil :: Value n -> Bool
isNil Nil = True
isNil _ = False

isCons :: Value n -> Bool
isCons (Cons _ _) = True
isCons _ = False

data ArgsSpec
  = ArgsSpec
      { positionalArgNames :: [Text]
      , remainingArgsName :: Maybe Text
      }
      deriving (Show, Read, Eq)

valToString :: Value n -> String
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
valToString (Native _) = "<<native>>"

valToText :: Value n -> Text
valToText = Text.pack . valToString

printConsTail :: Value n -> String
printConsTail Nil = ")"
printConsTail (Cons a b) = " " ++ valToString a ++ printConsTail b
printConsTail x = " . " ++ valToString x ++ ")"
