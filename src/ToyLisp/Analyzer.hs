{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module ToyLisp.Analyzer (analyseAst, expandMacro) where

import           Data.Maybe      (catMaybes)
import           ToyLisp.Runtime (MacroInfo (..))
import           ToyLisp.Syntax  (Ast (..), AstNode (..), SyntaxError (..),
                                  astNodePosition)

analyseAst :: Ast -> Either SyntaxError (Ast, [MacroInfo])
analyseAst (Ast nodes) = do
    checkTopLevelNodes nodes
    macros <- collectMacros nodes
    return (Ast nodes, macros)
    where
        checkTopLevelNodes = mapM_ (\case
            (ListNode _ _) -> Right ()
            node -> Left $ SyntaxError (astNodePosition node) "Top level expressions must be S-expressions")
        collectMacros = fmap catMaybes . mapM (\case
            (ListNode _ nodelist) -> case nodelist of
                SymbolNode _ "macrodef" : SymbolNode _ name : ListNode _ args : body ->
                    Right $ Just $ MacroInfo name args (Ast body)
                SymbolNode pos "defun" : SymbolNode _ "macrodef" : _ ->
                    Left $ SyntaxError pos "Cannot redefine macrodef"
                _ -> Right Nothing
            _ -> Right Nothing)

expandMacro :: (Ast, [MacroInfo]) -> Either SyntaxError Ast
expandMacro (Ast _nodes, _infos) =
    error "TODO"
