{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module ToyLisp.Analyzer (expandMacro, validateTopLevelNodes) where

import qualified Data.Text      as T
import           ToyLisp.Syntax (Ast (..), AstNode (..), SyntaxError (..),
                                 astNodePosition)

data MacroInfo = MacroInfo
    { macroName :: T.Text
    , macroArgs :: [AstNode]
    , macroBody :: Ast
    } deriving (Show, Eq)

validateTopLevelNodes :: [AstNode] -> Either SyntaxError ()
validateTopLevelNodes = mapM_ (\case
    (ListNode _ _) -> Right ()
    node -> Left $ SyntaxError (astNodePosition node) "Top level expressions must be S-expressions")

expandMacro :: (Ast, [MacroInfo]) -> Either SyntaxError Ast
expandMacro (Ast _nodes, _infos) =
    error "TODO"
