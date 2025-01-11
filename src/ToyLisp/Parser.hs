{-# LANGUAGE OverloadedRecordDot #-}

module ToyLisp.Parser (parse) where

import           Control.Exception      (assert)
import           Control.Monad          (void)
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Char              (isDigit, isLetter, isSpace)
import           ToyLisp.Syntax         (Ast (..), AstNode (..), TextRange (..),
                                         TextSize (..))

parse :: String -> Either TextSize Ast
parse input = do
    (result, finalState) <- runParser parseAstNodeList input
    assert (null finalState.input) (return $ Ast result)

data ParserState = ParserState
    { input    :: [Char]
    , position :: TextSize
    }

type Parser = StateT ParserState (ExceptT TextSize Identity)

runParser :: Parser a -> String -> Either TextSize (a, ParserState)
runParser parser str = runIdentity $ runExceptT $ runStateT parser ParserState { input = str, position = TextSize 0 }

----------------------

parseAstNodeList :: Parser [AstNode]
parseAstNodeList = do
    input <- gets input
    if null input
        then return []
        else do
            node <- parseAstNode
            nodes <- parseAstNodeList
            return $ node : nodes

parseAstNode :: Parser AstNode
parseAstNode = do
    eatWhitespace
    p <- get
    case p.input of
        '(' : _              -> parseList
        '\'' : _             -> parseQuote
        (c : _) | isDigit c  -> parseInt
        (c : _) | isLetter c -> parseSymbol
        _                    -> throwError p.position

parseList :: Parser AstNode
parseList = do
    startPos <- gets position
    assertCurrentChar '('
    advance 1
    eatWhitespace
    nodes <- parseAstNodeList
    eatWhitespace
    curChar <- gets (head . input)
    if curChar == ')'
        then do
            advance 1
            endPos <- gets position
            return $ ListNode (TextRange startPos endPos) nodes
        else do
            curPos <- gets position
            throwError curPos

parseQuote :: Parser AstNode
parseQuote = do
    startPos <- gets position
    assertCurrentChar '\''
    advance 1
    eatWhitespace
    node <- parseAstNode
    endPos <- gets position
    return $ ListNode (TextRange startPos endPos) [SymbolNode (TextRange startPos (startPos + 1)) "quote", node]

parseInt :: Parser AstNode
parseInt = do
    starPos <- gets position
    assertCurrentCharP isDigit
    number <- read <$> eatWhileP isDigit
    endPos <- gets position
    return $ IntNode (TextRange starPos endPos) number

parseSymbol :: Parser AstNode
parseSymbol = do
    startPos <- gets position
    assertCurrentCharP isLetter
    symbol <- eatWhileP isLetter
    endPos <- gets position
    return $ SymbolNode (TextRange startPos endPos) symbol

----------------------

advance :: Int -> Parser ()
advance n = modify $ \p -> ParserState { input = drop n p.input, position = p.position + TextSize n }

eatWhileP :: (Char -> Bool) -> Parser String
eatWhileP predicate = do
    input <- gets input
    let token = takeWhile predicate input
    advance (length token)
    return token

eatWhitespace :: Parser ()
eatWhitespace = void (eatWhileP isSpace)

assertCurrentChar :: Char -> Parser ()
assertCurrentChar c = assertCurrentCharP (== c)

assertCurrentCharP :: (Char -> Bool) -> Parser ()
assertCurrentCharP predicate = do
    input <- gets input
    let !_ = assert (not $ null input) ()
    let !_ = assert (predicate $ head input) ()
    return ()
