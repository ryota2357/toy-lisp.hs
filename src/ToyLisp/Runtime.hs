module ToyLisp.Runtime where

import qualified Data.Text      as T
import           ToyLisp.Syntax

data MacroInfo = MacroInfo
    { macroName :: T.Text
    , macroArgs :: [AstNode]
    , macroBody :: Ast
    } deriving (Show, Eq)
