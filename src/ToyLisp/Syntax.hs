module ToyLisp.Syntax where

newtype TextSize = TextSize Int deriving (Eq, Num)

instance Show TextSize where
    show (TextSize size) = show size

unTextSize :: TextSize -> Int
unTextSize (TextSize size) = size

data TextRange = TextRange TextSize TextSize deriving (Eq)

instance Show TextRange where
    show (TextRange (TextSize start) (TextSize end)) = show start ++ ".." ++ show end

unTextRange :: TextRange -> (Int, Int)
unTextRange (TextRange start end) = (unTextSize start, unTextSize end)

newtype Ast = Ast [AstNode]
    deriving (Eq, Show)

data AstNode
    = SymbolNode TextRange String
    | IntNode TextRange Integer
    -- | StringNode TextRange String
    -- | NilNode TextRange
    -- | TNode TextRange
    | ListNode TextRange [AstNode]
    -- | DottedListnode TextRange AstNode AstNode
    deriving (Eq, Show)
