{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module ToyLisp.Evaluator (eval) where

import           Control.Monad        (foldM, forM)
import           Control.Monad.Except (ExceptT, MonadError, runExceptT,
                                       throwError)
import           Control.Monad.State  (StateT, get, gets, lift, modify',
                                       runStateT)
import           Data.Function        (fix)
import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import qualified ToyLisp.Runtime      as RT
import           ToyLisp.Runtime      (Environment (..), ExecIO,
                                       FunctionInfo (..), LispObject (..),
                                       RuntimeError (..))
import           ToyLisp.Syntax       (Ast (..), AstNode (..), Symbol,
                                       TextRange, astNodePosition, unSymbol)

eval :: (ExecIO m) => Environment -> Ast -> m (Either RuntimeError LispObject, Environment)
eval env (Ast nodes) = do
    result <- runEvaluator $ mapM evalNode nodes
    pure $ case result of
        (Left err, finalEnv) -> (Left err, finalEnv)
        (Right objs, finalEnv) -> case objs of
            []    -> (Right $ LispList [], finalEnv)
            objs' -> (Right $ last objs', finalEnv)
  where
    runEvaluator evaluator = runStateT (runExceptT evaluator) env

type Evaluator m = ExceptT RuntimeError (StateT Environment m)

throwRuntimeError :: MonadError RuntimeError m => TextRange -> T.Text -> m a
throwRuntimeError pos msg = throwError $ RuntimeError pos msg

evalNode :: forall m. (ExecIO m) => AstNode -> Evaluator m LispObject
evalNode = \case
    IntNode _ i -> pure $ LispInt i
    FloatNode _ f -> pure $ LispFloat f
    StringNode _ s -> pure $ LispString s
    SymbolNode pos sym -> do
        env <- get
        case () of
            _ | Just obj <- M.lookup sym systemValueBindingsMap -> pure obj
              | Just obj <- RT.lookupFrameValueBinding sym env.currentLexicalFrame -> pure obj
              | Just obj <- RT.lookupFrameValueBinding sym env.currentSpecialFrame -> pure obj
              | Just obj <- RT.lookupValueBinding sym env.globalBindings -> pure obj
              | otherwise -> throwRuntimeError pos $ "Unbound symbol: " <> unSymbol sym
    ListNode _ [] -> pure $ LispList []
    ListNode _ (SymbolNode fnPos fnName : args) -> do
        env <- get
        case M.lookup fnName (systemFunctionBindingsMap @m) of
            Just fn -> do
                result <- fn args
                case result of
                    Right val -> pure val
                    Left err  -> throwRuntimeError fnPos err
            Nothing -> case RT.lookupFrameFunctionBinding fnName env.currentLexicalFrame of
                Just fnInfo -> callFunction fnInfo args
                Nothing     -> throwRuntimeError fnPos $ "Undefined function: " <> unSymbol fnName
    ListNode _ (notSymbolNode : _) ->
        throwRuntimeError (astNodePosition notSymbolNode) "Function name is not a symbol"

callFunction :: (ExecIO m) => FunctionInfo -> [AstNode] -> Evaluator m LispObject
callFunction = error "Not implemented"

systemValueBindingsMap :: M.Map Symbol LispObject
systemValueBindingsMap = M.fromList
    [ ("nil", LispList [])
    , ("pi", LispFloat pi)
    , ("t", LispTrue)
    ]

systemFunctionBindingsMap :: forall m. (ExecIO m) => M.Map Symbol ([AstNode] -> Evaluator m (Either T.Text LispObject))
systemFunctionBindingsMap = M.fromList
    [ ("+", \args -> do
        argValues <- mapM evalNode args
        pure $ foldM (\acc v -> case (acc, v) of
            (LispInt a, LispInt b)     -> Right $ LispInt (a + b)
            (LispFloat a, LispFloat b) -> Right $ LispFloat (a + b)
            (LispInt a, LispFloat b)   -> Right $ LispFloat (fromIntegral a + b)
            (LispFloat a, LispInt b)   -> Right $ LispFloat (a + fromIntegral b)
            _                          -> Left "Arguments are not numbers"
            ) (LispInt 0) argValues
      )
    , ("-", \args -> do
        argValues <- mapM evalNode args
        pure $ case argValues of
            [] -> Left $ mkInvalidArgCountErrorText "-" []
            [e] -> case e of
                LispInt i   -> Right $ LispInt (-i)
                LispFloat f -> Right $ LispFloat (-f)
                _           -> Left "Argument is not a number"
            e : es -> foldM (\acc v -> case (acc, v) of
                (LispInt a, LispInt b)     -> Right $ LispInt (a - b)
                (LispFloat a, LispFloat b) -> Right $ LispFloat (a - b)
                (LispInt a, LispFloat b)   -> Right $ LispFloat (fromIntegral a - b)
                (LispFloat a, LispInt b)   -> Right $ LispFloat (a - fromIntegral b)
                _                          -> Left "Arguments are not numbers"
                ) e es
      )
    , ("*", \args -> do
        argValues <- mapM evalNode args
        pure $ foldM (\acc v -> case (acc, v) of
            (LispInt a, LispInt b)     -> Right $ LispInt (a * b)
            (LispFloat a, LispFloat b) -> Right $ LispFloat (a * b)
            (LispInt a, LispFloat b)   -> Right $ LispFloat (fromIntegral a * b)
            (LispFloat a, LispInt b)   -> Right $ LispFloat (a * fromIntegral b)
            _                          -> Left "Arguments are not numbers"
            ) (LispInt 1) argValues
      )
    , ("/", \args -> do
        argValues <- mapM evalNode args
        let divideByZeroMsg = "Division by zero"
        pure $ case argValues of
            [] -> Left $ mkInvalidArgCountErrorText "/" []
            [e] -> case e of
                LispInt 0   -> Left divideByZeroMsg
                LispFloat 0 -> Left divideByZeroMsg
                LispInt i   -> Right $ LispFloat (1 / fromIntegral i)
                LispFloat f -> Right $ LispFloat (1 / f)
                _           -> Left "Argument is not a number"
            e : es -> foldM (\acc v -> case (acc, v) of
                (LispInt a, LispInt b)     | b /= 0 -> Right $ if a `mod` b == 0
                                                                then LispInt (a `div` b)
                                                                else LispFloat (fromIntegral a / fromIntegral b)
                (LispFloat a, LispFloat b) | b /= 0 -> Right $ LispFloat (a / b)
                (LispInt a, LispFloat b)   | b /= 0 -> Right $ LispFloat (fromIntegral a / b)
                (LispFloat a, LispInt b)   | b /= 0 -> Right $ LispFloat (a / fromIntegral b)
                (LispInt _, LispInt 0)     -> Left divideByZeroMsg
                (LispFloat _, LispFloat 0) -> Left divideByZeroMsg
                (LispInt _, LispFloat 0)   -> Left divideByZeroMsg
                (LispFloat _, LispInt 0)   -> Left divideByZeroMsg
                _                          -> Left "Arguments are not numbers"
                ) e es
      )
    , ("defun", \args -> case args of
        SymbolNode _ fnName : ListNode _ params : body -> if fnName `M.member` (systemFunctionBindingsMap @m)
            then pure $ Left $ "Redefining " <> unSymbol fnName <> " is not allowed"
            else case forM params (\case SymbolNode _ s -> Right s; _ -> Left ()) of
                Right params' -> do
                    frame <- gets currentLexicalFrame
                    let fnInfo = FunctionInfo params' (Ast body) frame
                    modify' $ RT.insertGlobalFunctionBinding fnName fnInfo
                    pure $ Right $ LispFunction fnInfo
                Left _ -> pure $ Left "Function parameters are not a list of symbols"
        SymbolNode _ _ : invalidParam | not $ null invalidParam ->
            pure $ Left "Function parameters are not a list"
        _ : _ : _ -> pure $ Left "Function name is not a symbol"
        _ -> pure $ Left $ mkInvalidArgCountErrorText "defun" args
      )
    , ("princ", \args -> do
        argValues <- mapM evalNode args
        case argValues of
            [arg] -> do
                let display = RT.displayLispObjectWith $ \case
                        LispString s -> Just $ T.unpack s
                        _            -> Nothing
                lift . lift $ RT.writeOutput $ display arg
                pure $ Right arg
            _ -> pure $ Left $ mkInvalidArgCountErrorText "princ" args
      )
    , ("quote", \args -> pure $ case args of
        [arg] -> Right $ fix (\quote -> \case
            IntNode _ i -> LispInt i
            FloatNode _ f -> LispFloat f
            StringNode _ s -> LispString s
            SymbolNode _ sym -> LispSymbol sym
            ListNode _ nodes -> LispList $ map quote nodes
            ) arg
        _ -> Left $ mkInvalidArgCountErrorText "quote" args
      )
    , ("setq", \args -> case args of
        [SymbolNode _ sym, value] -> if sym `M.member` systemValueBindingsMap
            then pure $ Left $ unSymbol sym <> " is a constant and thus cannot be set"
            else do
                value' <- evalNode value
                modify' $ RT.insertGlobalValueBinding sym value'
                pure $ Right value'
        [_, _] -> pure $ Left "Variable name is not a symbol"
        _ -> pure $ Left $ mkInvalidArgCountErrorText "setq" args
      )
    , ("type-of", \args -> do
        argValues <- mapM evalNode args
        pure $ case argValues of
            [arg] -> Right $ LispSymbol $ case arg of
                    LispInt _      -> "INTEGER"
                    LispFloat _    -> "FLOAT"
                    LispString _   -> "STRING"
                    LispSymbol _   -> "SYMBOL"
                    LispList _     -> "LIST"
                    LispFunction _ -> "FUNCTION"
                    LispTrue       -> "T"
            _ -> Left $ mkInvalidArgCountErrorText "type-of" args
      )
    ]
  where
    mkInvalidArgCountErrorText fnName args = "Invalid number of arguments for " <> unSymbol fnName <> ": " <> argsCountText
      where
        argsCountText = T.pack $ show $ length args
