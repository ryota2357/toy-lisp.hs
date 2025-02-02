{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module ToyLisp.Evaluator (eval) where

import           Control.Monad        (foldM, forM)
import           Control.Monad.Except (ExceptT, MonadError, runExceptT,
                                       throwError)
import           Control.Monad.State  (StateT, get, gets, lift, modify', put,
                                       runStateT)
import           Data.Function        (fix)
import           Data.List            (uncons)
import qualified Data.Map.Strict      as M
import qualified Data.Text            as T
import qualified ToyLisp.Runtime      as RT
import           ToyLisp.Runtime      (Environment (..), ExecIO,
                                       FunctionInfo (..), LexicalFrame (..),
                                       LispObject (..), RuntimeError (..))
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
    ListNode _ (SymbolNode fnPos fnName : args) ->
        case M.lookup fnName (systemFunctionBindingsMap @m) of
            Just fn -> do
                result <- fn args
                case result of
                    Right val -> pure val
                    Left err  -> throwRuntimeError fnPos err
            Nothing -> do
                lexicalFrame <- gets currentLexicalFrame
                case RT.lookupFrameFunctionBinding fnName lexicalFrame of
                    Just fnInfo -> do
                        argValues <- mapM evalNode args
                        result <- callFunction fnInfo argValues
                        case result of
                            Right val -> pure val
                            Left err  -> throwRuntimeError fnPos err
                    Nothing -> do
                        -- Evaluate arguments first because the global function may be defined/updated by the arguments themselves
                        argValues <- mapM evalNode args
                        globals <- gets globalBindings -- Get the updated global bindings
                        case RT.lookupFunctionBinding fnName globals of
                            Just fnInfo -> do
                                result <- callFunction fnInfo argValues
                                case result of
                                    Right val -> pure val
                                    Left err  -> throwRuntimeError fnPos err
                            Nothing -> throwRuntimeError fnPos $ "Undefined function: " <> unSymbol fnName
    ListNode _ (notSymbolNode : _) ->
        throwRuntimeError (astNodePosition notSymbolNode) "Function name is not a symbol"

callFunction :: forall m. (ExecIO m) => FunctionInfo -> [LispObject] -> Evaluator m (Either T.Text LispObject)
callFunction fn args =
    let argCount   = length args
        paramCount = length fn.functionParams
    in if argCount /= paramCount
        then
            let argCountText  = T.pack $ "got " ++ show argCount
                paramCountText = T.pack $ "expected " ++ show paramCount
            in pure $ Left $ "Wrong number of arguments: got " <> argCountText <> ", expected " <> paramCountText
        else do
            env <- get
            let nextEnv = env
                    { currentLexicalFrame = foldl
                        (\frame (param, arg) -> RT.insertValueBinding param arg frame)
                        fn.functionIntialFrame
                        (zip fn.functionParams args)
                    }
            (result, nextEnv') <- lift . lift $ eval nextEnv fn.functionBody
            case result of
                Left err -> throwError err
                Right obj  -> do
                    -- Restore the environment with the updated global bindings
                    put env { globalBindings = nextEnv'.globalBindings }
                    pure $ Right obj

systemValueBindingsMap :: M.Map Symbol LispObject
systemValueBindingsMap = M.fromList
    [ ("nil", LispList [])
    , ("pi", LispFloat pi)
    , ("t", LispTrue)
    ]

systemFunctionBindingsMap :: forall m. (ExecIO m) => M.Map Symbol ([AstNode] -> Evaluator m (Either T.Text LispObject))
systemFunctionBindingsMap = M.fromList (
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
            [] -> Left $ mkInvalidArgCountErrorText 0 ">= 1"
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
            [] -> Left $ mkInvalidArgCountErrorText 0 ">= 1"
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
    ]
    ++
    map (\(op, opI, opF) -> (op, \args -> do
        argValues <- mapM evalNode args
        let nil = Right $ LispList []
        let t = Right LispTrue
        let notNumber = Left "Argument is not a number"
        pure $ case argValues of
            [] -> Left $ mkInvalidArgCountErrorText 0 ">= 1"
            [LispInt _] -> t
            [LispFloat _] -> t
            [_] -> notNumber
            LispInt x : xs -> case () of
                _ | Right xs' <- forM xs (\case LispInt i -> Right i; _ -> Left ()) ->
                    if all (`opI` x) xs' then t else nil
                  | Right xs' <- forM xs (\case LispInt i -> Right $ fromIntegral i; LispFloat f -> Right f; _ -> Left ()) ->
                    if all (`opF` fromIntegral x) xs' then t else nil
                  | otherwise -> notNumber
            LispFloat x : xs -> case () of
                _ | Right xs' <- forM xs (\case LispInt i -> Right $ fromIntegral i; LispFloat f -> Right f; _ -> Left ()) ->
                    if all (`opF` x) xs' then t else nil
                  | otherwise -> notNumber
            _ -> notNumber
        ))
    [ ("=",  (==), (==))
    , ("/=", (/=), (/=))
    ]
    ++
    map (\(op, opI, opF) -> (op, \args -> do
        argValues <- mapM evalNode args
        let notNumber = Left "Argument is not a number"
        pure $ case argValues of
            [] -> Left $ mkInvalidArgCountErrorText 0 ">= 1"
            [LispInt _] -> Right LispTrue
            [LispFloat _] -> Right LispTrue
            [_] -> notNumber
            _ : xs -> case foldM (\_ v -> case v of
                    (LispInt a, LispInt b)     -> if a `opI` b then Right () else Left True
                    (LispFloat a, LispFloat b) -> if a `opF` b then Right () else Left True
                    (LispInt a, LispFloat b)   -> if fromIntegral a `opF` b then Right () else Left True
                    (LispFloat a, LispInt b)   -> if a `opF` fromIntegral b then Right () else Left True
                    _ -> Left False
                    ) () $ zip argValues xs
                of
                    Right _    -> Right LispTrue
                    Left True  -> Right $ LispList []
                    Left False -> notNumber
        ))
    [ (">", (>), (>))
    , ("<", (<), (<))
    , (">=", (>=), (>=))
    , ("<=", (<=), (<=))
    ]
    ++
    [ ("car", \args -> do
        argValues <- mapM evalNode args
        pure $ case argValues of
            [LispList []]      -> Right $ LispList []
            [LispList (x : _)] -> Right x
            [_]                -> Left "Argument is not a list"
            _                  -> Left $ mkInvalidArgCountErrorText (length args) "1"
      )
    , ("cdr", \args -> do
        argValues <- mapM evalNode args
        pure $ case argValues of
            [LispList []]       -> Right $ LispList []
            [LispList (_ : xs)] -> Right $ LispList xs
            [_]                -> Left "Argument is not a list"
            _                  -> Left $ mkInvalidArgCountErrorText (length args) "1"
      )
    , ("defun", \case
        SymbolNode _ fnName : ListNode _ params : body -> if fnName `M.member` (systemFunctionBindingsMap @m)
            then pure $ Left $ "Redefining " <> unSymbol fnName <> " is not allowed"
            else case forM params (\case SymbolNode _ s -> Right s; _ -> Left ()) of
                Right params' -> do
                    frame <- gets currentLexicalFrame
                    let fnInfo = FunctionInfo params' (Ast body) frame
                    modify' $ \env -> env
                        { globalBindings = RT.insertFunctionBinding fnName fnInfo env.globalBindings
                        }
                    pure $ Right $ LispFunction fnInfo
                Left _ -> pure $ Left "Function parameters are not a list of symbols"
        SymbolNode _ _ : invalidParam | not $ null invalidParam ->
            pure $ Left "Function parameters are not a list"
        _ : _ : _ -> pure $ Left "Function name is not a symbol"
        args -> pure $ Left $ mkInvalidArgCountErrorText (length args) ">= 2"
      )
    , ("if", \case
        [cond, thenExpr] -> do
            condValue <- evalNode cond
            case condValue of
                LispList [] -> pure $ Right $ LispList []
                _           -> Right <$> evalNode thenExpr
        [cond, thenExpr, elseExpr] -> do
            condValue <- evalNode cond
            Right <$> case condValue of
                LispList [] -> evalNode elseExpr
                _           -> evalNode thenExpr
        args -> pure $ Left $ mkInvalidArgCountErrorText (length args) "2 or 3"
      )
    , ("let", \case
        ListNode _ bindings : body -> do
            case forM bindings (\case
                    ListNode _ (SymbolNode _ sym : values) -> case uncons values of
                        Just (value, []) -> Right (sym, Just value)
                        Just (_, _) ->  Left $ "Binding for " <> unSymbol sym <> " is not a single value"
                        Nothing -> Right (sym, Nothing)
                    SymbolNode _ sym -> Right (sym, Nothing)
                    _ -> Left "Variable name is not a symbol")
                of
                Left err -> pure $ Left err
                Right bindings' -> do
                    nextBindingMap <- M.fromList <$> forM bindings' (
                        \(sym, value) -> (sym,) <$> case value of
                            Just value' -> evalNode value'
                            Nothing     -> pure $ LispList [])
                    env <- get
                    let nextEnv = env
                            { currentLexicalFrame = LexicalFrame
                                { lexicalValueBindings = nextBindingMap
                                , lexicalFunctionBindings = M.empty
                                , parentLexicalFrame = Just env.currentLexicalFrame
                                }
                            }
                    (result, nextEnv') <- lift . lift $ eval nextEnv (Ast body)
                    case result of
                        Left err -> throwError err
                        Right obj -> do
                            -- Restore the environment with the updated global bindings
                            put env { globalBindings = nextEnv'.globalBindings }
                            pure $ Right obj
        _ : _ -> pure $ Left "Bindings are not a list"
        args -> pure $ Left $ mkInvalidArgCountErrorText (length args) ">= 1"
      )
    , ("list", \args -> do
        argValues <- mapM evalNode args
        pure $ Right $ LispList argValues
      )
    , ("nthcdr", \args -> do
        argValues <- mapM evalNode args
        pure $ case argValues of
            [LispInt n, LispList xs] -> Right $ LispList $ drop (fromIntegral n) xs
            [LispInt _, _]           -> Left "Second argument is not a list"
            [_, _]                   -> Left "First argument is not an integer"
            _                        -> Left $ mkInvalidArgCountErrorText (length args) "2"
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
            _ -> pure $ Left $ mkInvalidArgCountErrorText (length args) "1"
      )
    , ("prog1", \args -> do
        argValues <- mapM evalNode args
        pure $ case uncons argValues of
            Just (first, _) -> Right first
            Nothing         -> Right $ LispList []
      )
    , ("progn", \args -> do
        argValues <- mapM evalNode args
        pure $ case argValues of
            [] -> Right $ LispList []
            _  -> Right $ last argValues
      )
    , ("quote", \args -> pure $ case args of
        [arg] -> Right $ fix (\quote -> \case
            IntNode _ i -> LispInt i
            FloatNode _ f -> LispFloat f
            StringNode _ s -> LispString s
            SymbolNode _ sym -> LispSymbol sym
            ListNode _ nodes -> LispList $ map quote nodes
            ) arg
        _ -> Left $ mkInvalidArgCountErrorText (length args) "1"
      )
    , ("setq", \case
        [SymbolNode _ sym, value] -> if sym `M.member` systemValueBindingsMap
            then pure $ Left $ unSymbol sym <> " is a constant and thus cannot be set"
            else do
                value' <- evalNode value
                modify' $ \env -> env
                    { globalBindings = RT.insertValueBinding sym value' env.globalBindings
                    }
                pure $ Right value'
        [_, _] -> pure $ Left "Variable name is not a symbol"
        args -> pure $ Left $ mkInvalidArgCountErrorText (length args) "2"
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
            _ -> Left $ mkInvalidArgCountErrorText (length args) "1"
      )
    ])
  where
    mkInvalidArgCountErrorText :: Int -> String -> T.Text
    mkInvalidArgCountErrorText given expected = "Wrong number of arguments: given " <> givenText <> ", expected " <> expectedText
      where
        givenText = T.pack $ show given
        expectedText = T.pack expected
