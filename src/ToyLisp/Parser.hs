{-# LANGUAGE OverloadedRecordDot #-}

module ToyLisp.Parser (parse) where

import           Control.Monad          (void)
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Char              (isDigit, isLetter, isSpace)
import           ToyLisp.Syntax
import           ToyLisp.Util           (assertAlways, safeHead)

parse :: String -> Either TextSize Ast
parse input = do
    (result, finalState) <- runParser parseAstNodeList input
    if null finalState.input
        then return $ Ast result
        else throwError finalState.position

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
    eatWhitespace
    input <- gets input
    if null input || head input == ')'
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
    assertCurrentCharP (== '(')
    startPos <- gets position
    advance 1
    eatWhitespace
    nodes <- parseAstNodeList
    eatWhitespace
    curChar <- gets (safeHead . input)
    if curChar == Just ')'
        then do
            advance 1
            endPos <- gets position
            return $ ListNode (TextRange startPos endPos) nodes
        else do
            curPos <- gets position
            throwError curPos

parseQuote :: Parser AstNode
parseQuote = do
    assertCurrentCharP (== '\'')
    startPos <- gets position
    advance 1
    eatWhitespace
    node <- parseAstNode
    endPos <- gets position
    return $ ListNode (TextRange startPos endPos) [SymbolNode (TextRange startPos (startPos + 1)) "quote", node]

parseInt :: Parser AstNode
parseInt = do
    assertCurrentCharP isDigit
    starPos <- gets position
    number <- read <$> eatWhileP isDigit
    endPos <- gets position
    return $ IntNode (TextRange starPos endPos) number

parseSymbol :: Parser AstNode
parseSymbol = do
    assertCurrentCharP isLetter
    startPos <- gets position
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

assertCurrentCharP :: (Char -> Bool) -> Parser ()
assertCurrentCharP predicate = do
    input <- gets input
    let !_ = assertAlways (not $ null input) ()
    let !_ = assertAlways (predicate $ head input) ()
    return ()
