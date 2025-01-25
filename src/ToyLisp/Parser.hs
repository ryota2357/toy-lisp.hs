{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module ToyLisp.Parser (parse) where

import           Control.Monad          (void)
import           Control.Monad.Except   (ExceptT, MonadError, runExceptT,
                                         throwError)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.State    (StateT, gets, modify, runStateT)
import           Data.Char              (isAscii, isSpace)
import           Data.Function          (fix)
import qualified Data.Text              as T
import           Text.Read              (readMaybe)
import           ToyLisp.Syntax         (Ast (..), AstNode (..),
                                         SyntaxError (..), TextRange (..),
                                         TextSize (..), mkSymbol)
import           ToyLisp.Util           (assertAlways, safeHead)

parse :: String -> Either SyntaxError Ast
parse input = do
    (result, finalState) <- runParser parseAstNodeList input
    if null finalState.input
        then return $ Ast result
        else throwSyntaxError finalState.position ("Unexpected character: " <> T.pack [head finalState.input])
  where
    runParser parser str = runIdentity $ runExceptT $ runStateT parser ParserState
        { input = str
        , position = TextSize 0
        }

data ParserState = ParserState
    { input    :: [Char]
    , position :: TextSize
    }

type Parser = StateT ParserState (ExceptT SyntaxError Identity)

throwSyntaxError :: MonadError SyntaxError m => TextSize -> T.Text -> m a
throwSyntaxError pos msg = throwError $ SyntaxError (TextRange pos (pos + 1)) msg

advance :: Int -> Parser ()
advance n = modify $ \p -> ParserState
    { input = drop n p.input
    , position = p.position + TextSize n
    }

eatWhileP :: (Char -> Bool) -> Parser String
eatWhileP predicate = do
    input <- gets input
    let token = takeWhile predicate input
    advance (length token)
    return token

eatWhitespace :: Parser ()
eatWhitespace = void (eatWhileP isSpace)

isIdentChar :: Char -> Bool
isIdentChar c = isAscii c && notElem c [' ', '"', '(', ')', '\'']

assertCurrentCharP :: (Char -> Bool) -> Parser ()
assertCurrentCharP predicate = do
    input <- gets input
    let !_ = assertAlways (not $ null input) ()
    let !_ = assertAlways (predicate $ head input) ()
    return ()

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
    gets (safeHead . input) >>= \case
        Just '(' -> parseList
        Just '\'' -> parseQuote
        Just '"' -> parseString
        Just c | isIdentChar c -> parseIdent
        mc -> do
            curPos <- gets position
            throwSyntaxError curPos (case mc of
                    Just c  -> "Unexpected character: " <> T.pack [c]
                    Nothing -> "Unexpected end of input")

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

parseString :: Parser AstNode
parseString = do
    assertCurrentCharP (== '"')
    startPos <- gets position
    advance 1
    str <- fix $ \loop -> do
        str <- eatWhileP (\c -> c /= '"' && c /= '\\')
        next <- gets (safeHead . input)
        if next == Just '\\'
            then do
                advance 1
                gets (safeHead . input) >>= \case
                    Just c | c `elem` ['"', '\\'] -> do
                        advance 1
                        rest <- loop
                        return $ str ++ [c] ++ rest
                    Just _ -> do
                        rest <- loop
                        return $ str ++  rest
                    Nothing -> return str
            else return str
    gets (safeHead . input) >>= \case
        Just '"' -> do
            advance 1
            endPos <- gets position
            return $ StringNode (TextRange startPos endPos) (T.pack str)
        _ -> do
            curPos <- gets position
            throwSyntaxError curPos "Expected '\"'"

parseIdent :: Parser AstNode
parseIdent = do
    assertCurrentCharP isIdentChar
    startPos <- gets position
    ident <- eatWhileP isIdentChar
    endPos <- gets position
    let range = TextRange startPos endPos
    return $ case () of
        _ | Just int <- readMaybe ident -> IntNode range int
          | Just float <- readMaybe ident -> FloatNode range float
          | otherwise -> SymbolNode range (mkSymbol $ T.pack ident)
