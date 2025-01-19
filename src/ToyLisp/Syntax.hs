{-# LANGUAGE LambdaCase #-}

module ToyLisp.Syntax where

import qualified Data.Text as T

newtype TextSize = TextSize Int
    deriving (Eq, Ord, Num, Bounded)

instance Show TextSize where
    show (TextSize size) = show size

data TextRange = TextRange TextSize TextSize
    deriving (Eq)

instance Show TextRange where
    show (TextRange (TextSize start) (TextSize end)) = show start ++ ".." ++ show end

data SyntaxError = SyntaxError
    { syntaxErrorPosition :: TextRange
    , syntaxErrorMessage  :: T.Text
    }
    deriving (Eq)

instance Show SyntaxError where
    show (SyntaxError range message) = "Syntax error at " ++ show range ++ ": " ++ T.unpack message

newtype Ast = Ast [AstNode]
    deriving (Eq, Show)

data AstNode
    = SymbolNode TextRange T.Text
    | IntNode TextRange Integer
    | FloatNode TextRange Double
    | StringNode TextRange T.Text
    | ListNode TextRange [AstNode]
    -- | DottedListNode TextRange AstNode AstNode
    deriving (Eq, Show)

getAstNodePosition :: AstNode -> TextRange
getAstNodePosition = \case
    SymbolNode range _ -> range
    IntNode range _ -> range
    FloatNode range _ -> range
    StringNode range _ -> range
    ListNode range _ -> range
