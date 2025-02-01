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
    { functionParams      :: [Symbol]
    , functionBody        :: Ast
    , functionIntialFrame :: LexicalFrame
    } deriving (Show, Eq)

class Bindings a where
    valueBindings :: a -> M.Map Symbol LispObject
    functionBindings :: a -> M.Map Symbol FunctionInfo

class Bindings a => Frame a where
    parentFrame :: a -> Maybe a

data LexicalFrame = LexicalFrame
    { lexicalValueBindings    :: M.Map Symbol LispObject
    , lexicalFunctionBindings :: M.Map Symbol FunctionInfo
    , parentLexicalFrame      :: Maybe LexicalFrame
    } deriving (Show, Eq)

instance Bindings LexicalFrame where
    valueBindings = lexicalValueBindings
    functionBindings = lexicalFunctionBindings

lookupValueBinding :: Bindings a => Symbol -> a -> Maybe LispObject
lookupValueBinding name bindings = M.lookup name (valueBindings bindings)

lookupFunctionBinding :: Bindings a => Symbol -> a -> Maybe FunctionInfo
lookupFunctionBinding name bindings = M.lookup name (functionBindings bindings)

instance Frame LexicalFrame where
    parentFrame = parentLexicalFrame

lookupFrameValueBinding :: Frame a => Symbol -> a -> Maybe LispObject
lookupFrameValueBinding name frame = case M.lookup name (valueBindings frame) of
    Just obj -> Just obj
    Nothing  -> case parentFrame frame of
        Just parent -> lookupFrameValueBinding name parent
        Nothing     -> Nothing

lookupFrameFunctionBinding :: Frame a => Symbol -> a -> Maybe FunctionInfo
lookupFrameFunctionBinding name frame = case M.lookup name (functionBindings frame) of
    Just obj -> Just obj
    Nothing  -> case parentFrame frame of
        Just parent -> lookupFrameFunctionBinding name parent
        Nothing     -> Nothing

data SpecialFrame = SpecialFrame
    { specialValueBindings    :: M.Map Symbol LispObject
    , specialFunctionBindings :: M.Map Symbol FunctionInfo
    , parentSpecialFrame      :: Maybe SpecialFrame
    } deriving (Show, Eq)

instance Bindings SpecialFrame where
    valueBindings = specialValueBindings
    functionBindings = specialFunctionBindings

instance Frame SpecialFrame where
    parentFrame = parentSpecialFrame

data GlobalBindings = GlobalBindings
    { globalValueBindings    :: M.Map Symbol LispObject
    , globalFunctionBindings :: M.Map Symbol FunctionInfo
    } deriving (Show, Eq)

instance Bindings GlobalBindings where
    valueBindings = globalValueBindings
    functionBindings = globalFunctionBindings

data Environment = Environment
    { globalBindings      :: GlobalBindings
    , currentLexicalFrame :: LexicalFrame
    , currentSpecialFrame :: SpecialFrame
    } deriving (Show, Eq)

emptyEnvironment :: Environment
emptyEnvironment = Environment
    { globalBindings = GlobalBindings M.empty M.empty
    , currentLexicalFrame = LexicalFrame M.empty M.empty Nothing
    , currentSpecialFrame = SpecialFrame M.empty M.empty Nothing
    }

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
