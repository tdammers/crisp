module Language.Crisp.Value
where

import qualified Data.Text (Text)
import Data.Text as Text
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

-- | Values we can represent
data Value
  = Nil -- ^ the empty list, doubling as the "null" value
  | Cons Value Value -- ^ a cons cell (Cons car cdr)
  | Bool Bool -- ^ a boolean (true or false)
  | Int Integer -- ^ integer number
  | String Text -- ^ unicode string
  | Bytes ByteString -- ^ byte array
  | Atom Text -- ^ a symbol; typically used to dereference variables
  | Lambda ArgsSpec Value -- ^ lambda function: Lambda args body
  deriving (Show, Read, Eq)

data ArgsSpec
  = ArgsSpec
      { positionalArgNames :: [Text]
      , remainingArgsName :: Text
      }
      deriving (Show, Read, Eq)
