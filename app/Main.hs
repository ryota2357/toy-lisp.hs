{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import qualified Data.ByteString.Char8 as BSC
import           Data.FileEmbed        (embedFileRelative)
import qualified Options.Applicative   as O
import qualified ToyLisp               as TL

main :: IO ()
main = O.execParser opts >>= runMain
  where
    opts = O.info (argumentParser O.<**> O.helper)
        ( O.fullDesc
        <> O.header "A simple Lisp interpreter written in Haskell" )

preludeFile :: String
preludeFile = BSC.unpack $ $(embedFileRelative "app/prelude.lisp")

runMain :: Arguments -> IO ()
runMain args =
    case args.scriptFilePath of
        Nothing -> TL.runReplWith config
        Just path -> do
            content <- readFile path -- TODO: Handle errors
            TL.runWith config content
  where
    config = TL.RunConfig
        { TL.runMode = if args.showAst then TL.ShowAstOnly else TL.ExecuteProgram
        , TL.preloadScripts = [TL.PreloadScript "prelude" preludeFile | not args.noPrelude]
        }

data Arguments = Arguments
    { scriptFilePath :: Maybe FilePath
    , noPrelude      :: Bool
    , showAst        :: Bool
    }

argumentParser :: O.Parser Arguments
argumentParser = Arguments
    <$> O.optional (O.strOption
        ( O.long "script"
        <> O.metavar "FILE"
        <> O.help "Script file to execute" ))
    <*> O.switch
        ( O.long "no-prelude"
        <> O.help "Do not load prelude")
    <*> O.switch
        ( O.long "show-ast"
        <> O.help "Only show AST without executing" )
