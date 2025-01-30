{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData          #-}

module ToyLisp.Runtime where

import qualified Data.Map.Strict as M
import qualified Data.Text       as T
import           System.IO       (hFlush, hPutStr, stderr, stdout)
import           ToyLisp.Syntax  (Ast, Symbol, TextRange, unSymbol)

class (Monad m) => ExecIO m where
    writeOutput   :: String -> m ()
    writeOutputLn :: String -> m ()
    writeOutputLn = writeOutput . (++ "\n")
    writeError   :: String -> m ()
    writeErrorLn :: String -> m ()
    writeErrorLn = writeError . (++ "\n")
    readInputLine :: m String

instance ExecIO IO where
    writeOutput = (>> hFlush stdout) . putStr
    writeError  = (>> hFlush stderr) . hPutStr stderr
    readInputLine = getLine

data LispObject
    = LispInt Integer
    | LispFloat Double
    | LispString T.Text
    | LispSymbol Symbol
    | LispList [LispObject]
    | LispFunction FunctionInfo
    | LispTrue
    deriving (Show, Eq)

displayLispObjectWith :: (LispObject -> Maybe String) -> LispObject -> String
displayLispObjectWith override obj = case override obj of
    Just str -> str
    Nothing  -> case obj of
        LispInt i      -> show i
        LispFloat f    -> show f
        LispString s   -> "\"" ++ T.unpack s ++ "\""
        LispSymbol s   -> T.unpack $ unSymbol s
        LispList []    -> "NIL"
        LispList xs    -> "(" ++ unwords (map (displayLispObjectWith override) xs) ++ ")"
        LispTrue       -> "T"
        LispFunction _ -> "<function>"

data FunctionInfo = FunctionInfo
    { functionArgs        :: [Symbol]
    , functionBody        :: Ast
    , functionIntialFrame :: LexicalFrame
    } deriving (Show, Eq)

data LexicalFrame = LexicalFrame
    { valueBindings      :: M.Map Symbol LispObject
    , functionBindings   :: M.Map Symbol FunctionInfo
    , parentLexicalFrame :: Maybe LexicalFrame
    } deriving (Show, Eq)

data SpecialFrame = SpecialFrame
    { specialBindings    :: M.Map Symbol LispObject
    , parentSpecialFrame :: Maybe SpecialFrame
    } deriving (Show, Eq)

data GlobalBindings = GlobalBindings
    { globalValueBindings    :: M.Map Symbol LispObject
    , globalFunctionBindings :: M.Map Symbol FunctionInfo
    } deriving (Show, Eq)

data Environment = Environment
    { globalBindings      :: GlobalBindings
    , currentLexicalFrame :: LexicalFrame
    , currentSpecialFrame :: SpecialFrame
    } deriving (Show, Eq)

emptyEnvironment :: Environment
emptyEnvironment = Environment
    { globalBindings = GlobalBindings M.empty M.empty
    , currentLexicalFrame = LexicalFrame M.empty M.empty Nothing
    , currentSpecialFrame = SpecialFrame M.empty Nothing
    }

lookupValueBinding :: Symbol -> Environment -> Maybe LispObject
lookupValueBinding symbol env = do
    case () of
        _ | Just obj <- lookupSpecialBindings symbol env.currentSpecialFrame -> Just obj
          | Just obj <- lookupLexicalBindings symbol env.currentLexicalFrame -> Just obj
          | Just obj <- lookupGlobalBindings symbol env.globalBindings -> Just obj
          | otherwise -> Nothing
  where
    lookupSpecialBindings name frame = case M.lookup name frame.specialBindings of
        Just obj -> Just obj
        Nothing  -> case frame.parentSpecialFrame of
            Just parent -> lookupSpecialBindings name parent
            Nothing     -> Nothing
    lookupLexicalBindings name frame = case M.lookup name frame.valueBindings of
        Just obj -> Just obj
        Nothing  -> case frame.parentLexicalFrame of
            Just parent -> lookupLexicalBindings name parent
            Nothing     -> Nothing
    lookupGlobalBindings name bindings = M.lookup name bindings.globalValueBindings

lookupFunctinBinding :: Symbol -> Environment -> Maybe FunctionInfo
lookupFunctinBinding symbol env = do
    case () of
         _| Just obj <- lookupLexicalBindings symbol env.currentLexicalFrame -> Just obj
          | Just obj <- lookupGlobalBindings symbol env.globalBindings -> Just obj
          | otherwise -> Nothing
  where
    lookupLexicalBindings name frame = case M.lookup name frame.functionBindings of
        Just obj -> Just obj
        Nothing  -> case frame.parentLexicalFrame of
            Just parent -> lookupLexicalBindings name parent
            Nothing     -> Nothing
    lookupGlobalBindings name bindings = M.lookup name bindings.globalFunctionBindings

insertGlobalValueBinding :: Symbol -> LispObject -> Environment -> Environment
insertGlobalValueBinding name value env = env
    { globalBindings = env.globalBindings
        { globalValueBindings = M.insert name value env.globalBindings.globalValueBindings
        }
    }

insertGlobalFunctionBinding :: Symbol -> FunctionInfo -> Environment -> Environment
insertGlobalFunctionBinding name value env = env
    { globalBindings = env.globalBindings
        { globalFunctionBindings = M.insert name value env.globalBindings.globalFunctionBindings
        }
    }

data RuntimeError = RuntimeError
    { runtimeErrorPosition :: TextRange
    , runtimeErrorMessage  :: T.Text
    } deriving (Eq)

instance Show RuntimeError where
    show (RuntimeError range message) = "Runtime error at " ++ show range ++ ": " ++ T.unpack message
