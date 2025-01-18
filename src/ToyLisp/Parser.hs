{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module ToyLisp.Parser (parse) where

import           Control.Monad          (void)
import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Char              (isAscii, isSpace)
import qualified Data.Text              as T
import           Text.Read              (readMaybe)
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
        '(' : _             -> parseList
        '\'' : _            -> parseQuote
        (c : _) | isIdent c -> parseIdent
        _                   -> throwError p.position

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

parseIdent :: Parser AstNode
parseIdent = do
    assertCurrentCharP isIdent
    startPos <- gets position
    ident <- eatWhileP isIdent
    endPos <- gets position
    let range = TextRange startPos endPos
    return $ case () of
        _ | Just int <- readMaybe ident -> IntNode range int
          | Just float <- readMaybe ident -> FloatNode range float
          | otherwise -> SymbolNode range (T.pack ident)

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

isIdent :: Char -> Bool
isIdent c = isAscii c && notElem c [' ', '"', '(', ')', '\'']

assertCurrentCharP :: (Char -> Bool) -> Parser ()
assertCurrentCharP predicate = do
    input <- gets input
    let !_ = assertAlways (not $ null input) ()
    let !_ = assertAlways (predicate $ head input) ()
    return ()
