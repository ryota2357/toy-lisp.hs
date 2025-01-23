module ToyLisp.Runtime where

import qualified Data.Text      as T
import           ToyLisp.Syntax


data LispObject
    = LispInt Integer
    | LispFloat Double
    | LispString T.Text
    | LispList [LispObject]
    -- | LispFunction ??
    | LispNil
    deriving ()

data RuntimeError = RuntimeError
    { runtimeErrorPosition :: TextRange
    , runtimeErrorMessage  :: T.Text
    }
    deriving (Eq)

instance Show RuntimeError where
    show (RuntimeError range message) = "Runtime error at " ++ show range ++ ": " ++ T.unpack message
