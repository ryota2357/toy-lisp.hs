{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module ToyLisp.Parser (parse) where

import           Control.Monad          (void)
import           Control.Monad.Except   (ExceptT, MonadError, runExceptT,
                                         throwError)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.State    (StateT, get, gets, modify, runStateT)
import           Data.Char              (isAscii, isSpace)
import qualified Data.Text              as T
import           Text.Read              (readMaybe)
import           ToyLisp.Syntax         (Ast (..), AstNode (..),
                                         SyntaxError (..), TextRange (..),
                                         TextSize (..))
import           ToyLisp.Util           (assertAlways, safeHead)

parse :: String -> Either SyntaxError Ast
parse input = do
    (result, finalState) <- runParser parseAstNodeList input
    if null finalState.input
        then return $ Ast result
        else throwSyntaxError finalState.position "Unexpected character"

data ParserState = ParserState
    { input    :: [Char]
    , position :: TextSize
    }

type Parser = StateT ParserState (ExceptT SyntaxError Identity)

runParser :: Parser a -> String -> Either SyntaxError (a, ParserState)
runParser parser str = runIdentity $ runExceptT $ runStateT parser ParserState { input = str, position = TextSize 0 }

throwSyntaxError :: MonadError SyntaxError m => TextSize -> T.Text -> m a
throwSyntaxError pos msg = throwError $ SyntaxError (TextRange pos (pos + 1)) msg

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
        _                   -> throwSyntaxError p.position "Unexpected character"

parseList :: Parser AstNode
parseList = do
    assertCurrentCharP (== '(')
    startPos <- gets position
    advance 1
    eatWhitespace
    nodes <- parseAstNodeList
    eatWhitespace
    gets (safeHead . input) >>= \case
        Just ')' -> do
            advance 1
            endPos <- gets position
            return $ ListNode (TextRange startPos endPos) nodes
        _ -> do
            curPos <- gets position
            throwSyntaxError curPos "Expected ')'"

parseQuote :: Parser AstNode
parseQuote = do
    assertCurrentCharP (== '\'')
    startPos <- gets position
    advance 1
    eatWhitespace
    node <- parseAstNode
    endPos <- gets position
    let quote = SymbolNode (TextRange startPos (startPos + 1)) "quote"
    return $ ListNode (TextRange startPos endPos) [quote, node]

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
