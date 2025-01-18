module ToyLisp.Syntax where

import qualified Data.Text as T

newtype TextSize = TextSize Int
    deriving (Eq, Ord, Num, Bounded)
instance Show TextSize where
    show (TextSize size) = show size

unTextSize :: TextSize -> Int
unTextSize (TextSize size) = size

data TextRange = TextRange TextSize TextSize
    deriving (Eq)

instance Show TextRange where
    show (TextRange (TextSize start) (TextSize end)) = show start ++ ".." ++ show end

unTextRange :: TextRange -> (Int, Int)
unTextRange (TextRange start end) = (unTextSize start, unTextSize end)

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
