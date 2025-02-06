{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings   #-}

module ToyLisp.Evaluator (eval) where

import           Control.Monad        (foldM, forM)
import           Control.Monad.Except (ExceptT, MonadError, runExceptT,
                                       throwError)
import           Control.Monad.State  (StateT, get, gets, modify', put,
                                       runStateT)
import           Control.Monad.Trans  (lift)
import qualified Data.Bifunctor       as BF
import           Data.Function        (fix)
import           Data.List            (partition, uncons)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (isJust, isNothing)
import qualified Data.Text            as T
import qualified ToyLisp.Runtime      as RT
import           ToyLisp.Runtime      (Environment (..), ExecIO,
                                       FunctionInfo (..), LexicalFrame (..),
                                       LispObject (..), RuntimeError (..),
                                       SpecialFrame (..))
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
                        -- Since the global function may be defined/updated in the arguments possition,
                        -- We need to evaluate the arguments first
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

throwWrongNumberOfArgsError :: MonadError T.Text m' => Int -> String -> m' a -- m is already used
throwWrongNumberOfArgsError given expected = throwError errorText
  where
    givenText = T.pack $ show given
    expectedText = T.pack expected
    errorText = "Wrong number of arguments: given " <> givenText <> ", expected " <> expectedText

callFunction :: forall m. (ExecIO m) => FunctionInfo -> [LispObject] -> Evaluator m (Either T.Text LispObject)
callFunction fn args =
    let argCount   = length args
        paramCount = length fn.functionParams
    in runExceptT $ if argCount /= paramCount
        then
            throwWrongNumberOfArgsError argCount (show paramCount)
        else do
            env <- get
            let nextEnv = env
                    { currentLexicalFrame = foldl
                        (\frame (param, arg) -> RT.insertValueBinding param arg frame)
                        fn.functionIntialFrame
                        (zip fn.functionParams args)
                    }
            (result, nextEnv') <- lift . lift . lift $ eval nextEnv fn.functionBody
            case result of
                Left err -> lift $ throwError err
                Right obj  -> do
                    -- Restore the environment with the updated global bindings
                    put env { globalBindings = nextEnv'.globalBindings }
                    pure obj

systemValueBindingsMap :: M.Map Symbol LispObject
systemValueBindingsMap = M.fromList
    [ ("nil", LispList [])
    , ("pi", LispFloat pi)
    , ("t", LispTrue)
    ]

systemFunctionBindingsMap :: forall m. (ExecIO m) => M.Map Symbol ([AstNode] -> Evaluator m (Either T.Text LispObject))
systemFunctionBindingsMap = M.fromList $ map (BF.second (runExceptT <$>)) (
    [ ("+", \args -> do
        argValues <- lift $ mapM evalNode args
        foldM (\acc v -> case (acc, v) of
            (LispInt a, LispInt b)     -> pure $ LispInt (a + b)
            (LispFloat a, LispFloat b) -> pure $ LispFloat (a + b)
            (LispInt a, LispFloat b)   -> pure $ LispFloat (fromIntegral a + b)
            (LispFloat a, LispInt b)   -> pure $ LispFloat (a + fromIntegral b)
            _                          -> throwError "Arguments are not numbers"
            ) (LispInt 0) argValues
      )
    , ("-", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [] -> throwWrongNumberOfArgsError 0 ">= 1"
            [e] -> case e of
                LispInt i   -> pure $ LispInt (-i)
                LispFloat f -> pure $ LispFloat (-f)
                _           -> throwError "Argument is not a number"
            e : es -> foldM (\acc v -> case (acc, v) of
                (LispInt a, LispInt b)     -> pure $ LispInt (a - b)
                (LispFloat a, LispFloat b) -> pure $ LispFloat (a - b)
                (LispInt a, LispFloat b)   -> pure $ LispFloat (fromIntegral a - b)
                (LispFloat a, LispInt b)   -> pure $ LispFloat (a - fromIntegral b)
                _                          -> throwError "Arguments are not numbers"
                ) e es
      )
    , ("*", \args -> do
        argValues <- lift $ mapM evalNode args
        foldM (\acc v -> case (acc, v) of
            (LispInt a, LispInt b)     -> pure $ LispInt (a * b)
            (LispFloat a, LispFloat b) -> pure $ LispFloat (a * b)
            (LispInt a, LispFloat b)   -> pure $ LispFloat (fromIntegral a * b)
            (LispFloat a, LispInt b)   -> pure $ LispFloat (a * fromIntegral b)
            _                          -> throwError "Arguments are not numbers"
            ) (LispInt 1) argValues
      )
    , ("/", \args -> do
        argValues <- lift $ mapM evalNode args
        let divideByZeroMsg = "Division by zero"
        case argValues of
            [] -> throwWrongNumberOfArgsError 0 ">= 1"
            [e] -> case e of
                LispInt 0   -> throwError divideByZeroMsg
                LispFloat 0 -> throwError divideByZeroMsg
                LispInt i   -> pure $ LispFloat (1 / fromIntegral i)
                LispFloat f -> pure $ LispFloat (1 / f)
                _           -> throwError "Argument is not a number"
            e : es -> foldM (\acc v -> case (acc, v) of
                (LispInt a, LispInt b)     | b /= 0 -> pure $ if a `mod` b == 0
                                                                then LispInt (a `div` b)
                                                                else LispFloat (fromIntegral a / fromIntegral b)
                (LispFloat a, LispFloat b) | b /= 0 -> pure $ LispFloat (a / b)
                (LispInt a, LispFloat b)   | b /= 0 -> pure $ LispFloat (fromIntegral a / b)
                (LispFloat a, LispInt b)   | b /= 0 -> pure $ LispFloat (a / fromIntegral b)
                (LispInt _, LispInt 0)     -> throwError divideByZeroMsg
                (LispFloat _, LispFloat 0) -> throwError divideByZeroMsg
                (LispInt _, LispFloat 0)   -> throwError divideByZeroMsg
                (LispFloat _, LispInt 0)   -> throwError divideByZeroMsg
                _                          -> throwError "Arguments are not numbers"
                ) e es
      )
    ]
    ++
    map (\(op, opI, opF) -> (op, \args -> do
        argValues <- lift $ mapM evalNode args
        let t = LispTrue; nil = LispList []
        let notNumberMsg = "Argument is not a number"
        case argValues of
            [] -> throwWrongNumberOfArgsError 0 ">= 1"
            [LispInt _] -> pure t
            [LispFloat _] -> pure t
            [_] -> throwError notNumberMsg
            LispInt x : xs -> case () of
                _ | Right xs' <- forM xs (\case LispInt i -> Right i; _ -> Left ()) ->
                    pure $ if all (`opI` x) xs' then t else nil
                  | Right xs' <- forM xs (\case LispInt i -> Right $ fromIntegral i; LispFloat f -> Right f; _ -> Left ()) ->
                    pure $ if all (`opF` fromIntegral x) xs' then t else nil
                  | otherwise -> throwError notNumberMsg
            LispFloat x : xs -> case () of
                _ | Right xs' <- forM xs (\case LispInt i -> Right $ fromIntegral i; LispFloat f -> Right f; _ -> Left ()) ->
                    pure $ if all (`opF` x) xs' then t else nil
                  | otherwise -> throwError notNumberMsg
            _ -> throwError notNumberMsg
        ))
    [ ("=",  (==), (==))
    , ("/=", (/=), (/=))
    ]
    ++
    map (\(op, opI, opF) -> (op, \args -> do
        argValues <- lift $ mapM evalNode args
        let notNumberMsg = "Argument is not a number"
        case argValues of
            [] -> throwWrongNumberOfArgsError 0 ">= 1"
            [LispInt _] -> pure LispTrue
            [LispFloat _] -> pure LispTrue
            [_] -> throwError notNumberMsg
            _ : xs -> case foldM (\_ v -> case v of
                    (LispInt a, LispInt b)     -> if a `opI` b then Right () else Left True
                    (LispFloat a, LispFloat b) -> if a `opF` b then Right () else Left True
                    (LispInt a, LispFloat b)   -> if fromIntegral a `opF` b then Right () else Left True
                    (LispFloat a, LispInt b)   -> if a `opF` fromIntegral b then Right () else Left True
                    _ -> Left False
                    ) () $ zip argValues xs
                of
                    Right _    -> pure LispTrue
                    Left True  -> pure $ LispList []
                    Left False -> throwError notNumberMsg
        ))
    [ (">", (>), (>))
    , ("<", (<), (<))
    , (">=", (>=), (>=))
    , ("<=", (<=), (<=))
    ]
    ++
    [ ("and", lift . fix (\next -> \case
        [] -> pure LispTrue
        arg : args -> do
            value <- evalNode arg
            if value == LispList []
                then pure $ LispList []
                else next args
        )
      )
    , ("car", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [LispList []]      -> pure $ LispList []
            [LispList (x : _)] -> pure x
            [_]                -> throwError "Argument is not a list"
            _                  -> throwWrongNumberOfArgsError (length args) "1"
      )
    , ("cdr", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [LispList []]       -> pure $ LispList []
            [LispList (_ : xs)] -> pure $ LispList xs
            [_]                 -> throwError "Argument is not a list"
            _                   -> throwWrongNumberOfArgsError (length args) "1"
      )
    , ("defparameter", \case
        [SymbolNode _ symbol, value] -> do
            value' <- lift $ evalNode value
            modify' $ \env -> env
                { currentSpecialFrame = insertRecent symbol value' env.currentSpecialFrame
                }
            pure $ LispSymbol symbol
          where
            insertRecent sym obj frame = case RT.lookupValueBinding sym frame of
                Just _ -> RT.insertValueBinding sym obj frame  -- Update the value
                Nothing -> case frame.parentSpecialFrame of
                    Just parent -> frame { parentSpecialFrame = Just $ insertRecent sym obj parent } -- Continue searching
                    Nothing     -> RT.insertValueBinding sym obj frame -- Insert to the root frame
        [_, _] -> throwError "Variable name is not a symbol"
        args -> throwWrongNumberOfArgsError (length args) "2"
      )
    , ("defun", \case
        SymbolNode _ fnName : ListNode _ params : body -> if fnName `M.member` (systemFunctionBindingsMap @m)
            then throwError $ "Redefining " <> unSymbol fnName <> " is not allowed"
            else case forM params (\case SymbolNode _ s -> Right s; _ -> Left ()) of
                Right params' -> do
                    frame <- gets currentLexicalFrame
                    let fnInfo = FunctionInfo params' (Ast body) frame
                    modify' $ \env -> env
                        { globalBindings = RT.insertFunctionBinding fnName fnInfo env.globalBindings
                        }
                    pure $ LispFunction fnInfo
                Left _ -> throwError "Function parameters are not a list of symbols"
        SymbolNode _ _ : invalidParam | not $ null invalidParam ->
            throwError "Function parameters are not a list"
        _ : _ : _ -> throwError "Function name is not a symbol"
        args -> throwWrongNumberOfArgsError (length args) ">= 2"
      )
    , ("defvar", \case
        SymbolNode _ symbol : values | null values || length values == 1 -> do
            value <- lift $ case uncons values of
                Just (value, _) -> evalNode value
                Nothing         -> pure $ LispList []
            modify' $ \env -> env
                { currentSpecialFrame = insertIfNotExists symbol value env.currentSpecialFrame
                }
            pure $ LispSymbol symbol
          where
            insertIfNotExists sym obj frame = case RT.lookupValueBinding sym frame of
                Just _ -> frame  -- Do nothing if the symbol already exists
                Nothing -> case frame.parentSpecialFrame of
                    Just parent -> frame { parentSpecialFrame = Just $ insertIfNotExists sym obj parent } -- Continue searching
                    Nothing     -> RT.insertValueBinding sym obj frame  -- Insert to the root frame
        [_] -> throwError "Variable name is not a symbol"
        args -> throwWrongNumberOfArgsError (length args) "1 or 2"
      )
    , ("eq", \args -> do
        argValues <- lift $ mapM evalNode args
        let t =  LispTrue; nil = LispList []
        case argValues of
            [LispList [], LispList []] -> pure t
            [LispList _, _] -> pure nil
            [_, LispList _] -> pure nil
            [LispString _, _] -> pure nil
            [_, LispString _] -> pure nil
            [a, b] -> pure $ if a == b then t else nil
            _      -> throwWrongNumberOfArgsError (length args) "2"
      )
    , ("eql", \args -> do
        argValues <- lift $ mapM evalNode args
        let t =  LispTrue; nil = LispList []
        case argValues of
            [LispList [], LispList []] -> pure t
            [LispList _, _] -> pure nil
            [_, LispList _] -> pure nil
            [a, b] -> pure $ if a == b then t else nil
            _      -> throwWrongNumberOfArgsError (length args) "2"
      )
    , ("equal", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [a, b] -> pure $ if a == b then LispTrue else LispList []
            _      -> throwWrongNumberOfArgsError (length args) "1"
      )
    , ("equalp", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [a, b] -> pure $
                if fix (\equalp -> \case
                    (LispString s1, LispString s2) -> T.toLower s1 == T.toLower s2
                    (LispList xs, LispList ys) -> (length xs == length ys && all equalp (zip xs ys))
                    (LispInt x, LispFloat y) -> fromIntegral x == y
                    (LispFloat x, LispInt y) -> x == fromIntegral y
                    (x, y) ->  x == y
                ) (a, b)
                    then LispTrue
                    else LispList []
            _ -> throwWrongNumberOfArgsError (length args) "2"
      )
    , ("funcall", \case
        fnIndirect : args -> (lift . evalNode) fnIndirect >>= \case
            LispFunction fn -> do
                argValues <- lift $ mapM evalNode args
                result <- lift $ callFunction fn argValues
                case result of
                    Right val -> pure val
                    Left err  -> throwError err
            LispSystemFunction fnName -> do
                case M.lookup fnName (systemFunctionBindingsMap @m) of
                    Just fn -> do
                        result <- lift $ fn args
                        case result of
                            Right val -> pure val
                            Left err  -> throwError err
                    Nothing ->
                        -- LispSystemFunction is created by the system such as `function` function.
                        -- The prosess of creating LispSystemFunction ensures that the function is exist,
                        -- so if we reach here, it is an bug.
                        error "unreachable: LispSystemFunction should be defined in systemFunctionBindingsMap"
            LispSymbol symbol -> case M.lookup symbol (systemFunctionBindingsMap @m) of
                Just fn -> do
                    result <- lift $ fn args
                    case result of
                        Right val -> pure val
                        Left err  -> throwError err
                Nothing -> do
                    -- Since the global function may be defined/updated in the arguments possition,
                    -- We need to evaluate the arguments first
                    argValues <- lift $ mapM evalNode args
                    globals <- gets globalBindings -- Get the updated global bindings
                    case RT.lookupFunctionBinding symbol globals of
                        Just fn -> do
                            result <- lift $ callFunction fn argValues
                            case result of
                                Right val -> pure val
                                Left err  -> throwError err
                        Nothing -> throwError $ "Undefined global function: " <> unSymbol symbol
            _ -> throwError "Argument is not a function"
        [] -> throwWrongNumberOfArgsError 0 ">= 1"
      )
    , ("function", \case
        [SymbolNode _ fnName] -> do
            env <- get
            case () of
                _ | Just _  <- M.lookup fnName (systemFunctionBindingsMap @m) -> pure $ LispSystemFunction fnName
                  | Just fn <- RT.lookupFrameFunctionBinding fnName env.currentLexicalFrame -> pure $ LispFunction fn
                  | Just fn <- RT.lookupFrameFunctionBinding fnName env.currentSpecialFrame -> pure $ LispFunction fn
                  | Just fn <- RT.lookupFunctionBinding fnName env.globalBindings -> pure $ LispFunction fn
                  | otherwise -> throwError $ "Undefined function: " <> unSymbol fnName
        [lambda@(ListNode _ (SymbolNode _ "lambda" : _))] -> lift $ evalNode lambda
        _ -> throwError "Function name is not a symbol"
      )
    , ("if", \case
        [cond, thenExpr] -> do
            condValue <- lift $ evalNode cond
            case condValue of
                LispList [] -> pure $ LispList []
                _           -> lift $ evalNode thenExpr
        [cond, thenExpr, elseExpr] -> do
            condValue <- lift $ evalNode cond
            lift $ case condValue of
                LispList [] -> evalNode elseExpr
                _           -> evalNode thenExpr
        args -> throwWrongNumberOfArgsError (length args) "2 or 3"
      )
    , ("lambda", \case
        ListNode _ params : body ->
            case forM params (\case SymbolNode _ s -> Right s; _ -> Left ()) of
                Right params' -> do
                    frame <- gets currentLexicalFrame
                    pure $ LispFunction $ FunctionInfo params' (Ast body) frame
                Left _ -> throwError "Function parameters are not a list of symbols"
        _ : _ -> throwError "Function parameters are not a list"
        args -> throwWrongNumberOfArgsError (length args) ">= 1"
      )
    , ("let", \case
        ListNode _ bindings : body -> do
            env <- get
            (lexicalBindings, specialBindings) <- partition (\(sym, _) ->
                -- If exits in the current lexical frame, it is a lexical binding
                -- Else if exits in the current special frame, it is a special binding
                -- Otherwise, it is a lexical binding
                isJust (RT.lookupFrameValueBinding sym env.currentLexicalFrame)
                || isNothing (RT.lookupFrameValueBinding sym env.currentSpecialFrame)
                ) <$> evalBindings bindings
            let nextEnv = env
                    { currentLexicalFrame = if null lexicalBindings
                        then env.currentLexicalFrame
                        else LexicalFrame
                            { lexicalValueBindings = M.fromList lexicalBindings
                            , lexicalFunctionBindings = M.empty
                            , parentLexicalFrame = Just env.currentLexicalFrame
                            }
                    , currentSpecialFrame = if null specialBindings
                        then env.currentSpecialFrame
                        else SpecialFrame
                            { specialValueBindings = M.fromList specialBindings
                            , specialFunctionBindings = M.empty
                            , parentSpecialFrame = Just env.currentSpecialFrame
                            }
                    }
            (result, nextEnv') <- lift . lift . lift $ eval nextEnv (Ast body)
            case result of
                Left err  -> lift $ throwError err
                Right obj -> do
                    -- Restore the environment with the updated global bindings
                    put env { globalBindings = nextEnv'.globalBindings }
                    pure obj
          where
            evalBindings = mapM (\case
                ListNode _ (SymbolNode _ sym : values) -> case uncons values of
                    Just (value, []) -> do
                        value' <- lift $ evalNode value
                        pure (sym, value')
                    Just (_, _) -> throwError $ "Binding for " <> unSymbol sym <> " is not a single value"
                    Nothing -> pure (sym, LispList [])
                SymbolNode _ sym -> pure (sym, LispList [])
                _ -> throwError "Variable name is not a symbol")
        _ : _ -> throwError "Bindings are not a list"
        args -> throwWrongNumberOfArgsError (length args) ">= 1"
      )
    , ("list", \args -> do
        argValues <- lift $ mapM evalNode args
        pure $ LispList argValues
      )
    , ("nthcdr", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [LispInt n, LispList xs] -> pure $ LispList $ drop (fromIntegral n) xs
            [LispInt _, _]           -> throwError "Second argument is not a list"
            [_, _]                   -> throwError "First argument is not an integer"
            _                        -> throwWrongNumberOfArgsError (length args) "2"
      )
    , ("or", lift . fix (\next -> \case
        [] -> pure $ LispList []
        arg : args -> do
            value <- evalNode arg
            if value == LispList []
                then next args
                else pure value
        )
      )
    , ("princ", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [arg] -> do
                let display = RT.displayLispObjectWith $ \case
                        LispString s -> Just $ T.unpack s
                        _            -> Nothing
                lift . lift . lift $ RT.writeOutput $ display arg
                pure  arg
            _ -> throwWrongNumberOfArgsError (length args) "1"
      )
    , ("prog1", \args -> do
        argValues <- lift $ mapM evalNode args
        pure $ case uncons argValues of
            Just (first, _) -> first
            Nothing         -> LispList []
      )
    , ("progn", \args -> do
        argValues <- lift $ mapM evalNode args
        pure $ case argValues of
            [] -> LispList []
            _  -> last argValues
      )
    , ("quote", \args -> case args of
        [arg] -> pure $ fix (\quote -> \case
            IntNode _ i -> LispInt i
            FloatNode _ f -> LispFloat f
            StringNode _ s -> LispString s
            SymbolNode _ sym -> LispSymbol sym
            ListNode _ nodes -> LispList $ map quote nodes
            ) arg
        _ -> throwWrongNumberOfArgsError (length args) "1"
      )
    , ("setq", \args -> if odd (length args)
        then throwWrongNumberOfArgsError (length args) "even number"
        else foldM (\_ -> \case
            (SymbolNode _ symbol, value) -> if symbol `M.member` systemValueBindingsMap
                then throwError $ "Redefining " <> unSymbol symbol <> " is not allowed"
                else do
                    value' <- lift $ evalNode value
                    env <- get
                    case () of
                        _ | Just _ <- RT.lookupFrameValueBinding symbol env.currentLexicalFrame -> put env
                                { currentLexicalFrame = findInsertL symbol value' env.currentLexicalFrame
                                }
                          | Just _ <- RT.lookupFrameValueBinding symbol env.currentSpecialFrame -> put env
                                { currentSpecialFrame = findInsertS symbol value' env.currentSpecialFrame
                                }
                          | otherwise -> put env
                                { globalBindings = RT.insertValueBinding symbol value' env.globalBindings
                                }
                    pure value'
                  where
                    findInsertL sym obj frame = case RT.lookupValueBinding sym frame of
                        Just _ -> RT.insertValueBinding sym obj frame
                        Nothing -> case frame.parentLexicalFrame of
                            Just parent -> frame { parentLexicalFrame = Just $ findInsertL sym obj parent }
                            Nothing     -> frame
                    findInsertS sym obj frame = case RT.lookupValueBinding sym frame of
                        Just _ -> RT.insertValueBinding sym obj frame
                        Nothing -> case frame.parentSpecialFrame of
                            Just parent -> frame { parentSpecialFrame = Just $ findInsertS sym obj parent }
                            Nothing     -> frame
            _ -> throwError "Variable name is not a symbol")
            (LispList [])
            $ fix (\mkPairs -> \case
                [] -> []
                x : y : xs -> (x, y) : mkPairs xs
                _ -> error "unreachable" -- We have already checked the length of `args` is even
                ) args
      )
    , ("type-of", \args -> do
        argValues <- lift $ mapM evalNode args
        case argValues of
            [arg] -> pure $ LispSymbol $ case arg of
                    LispInt _            -> "INTEGER"
                    LispFloat _          -> "FLOAT"
                    LispString _         -> "STRING"
                    LispSymbol _         -> "SYMBOL"
                    LispList []          -> "NULL"
                    LispList _           -> "LIST"
                    LispFunction _       -> "FUNCTION"
                    LispTrue             -> "T"
                    LispSystemFunction _ -> "FUNCTION"
            _ -> throwWrongNumberOfArgsError (length args) "1"
      )
    ])
