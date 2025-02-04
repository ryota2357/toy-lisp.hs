{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module ToyLisp.Analyzer (validateAst) where

import           Control.Monad  (forM_)
import           ToyLisp.Syntax (Ast (..), AstNode (..), SyntaxError (..),
                                 astNodePosition)

validateAst :: Ast -> Either SyntaxError ()
validateAst (Ast nodes) = forM_ nodes (\case
    (ListNode _ _) -> Right ()
    node -> Left $ SyntaxError (astNodePosition node) "Top level expressions must be S-expressions")

-- expandMacro :: (Ast, [MacroInfo]) -> Either SyntaxError Ast
-- expandMacro (Ast _nodes, _infos) =
--     error "TODO"
