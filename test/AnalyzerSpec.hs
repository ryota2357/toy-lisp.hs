{-# LANGUAGE OverloadedStrings #-}

module AnalyzerSpec (spec) where

import           Test.Hspec
import           ToyLisp.Analyzer (analyseAst)
import           ToyLisp.Runtime  (MacroInfo (..))
import           ToyLisp.Syntax

spec :: Spec
spec = do
    describe "analyseAst" $ do
        it "empty input" $
            analyseAst (Ast []) `shouldBe` Right (Ast [], [])

        it "top-level must be lists" $
            analyseAst (Ast [SymbolNode (TextRange 0 1) "a"]) `shouldBe`
                Left (SyntaxError (TextRange 0 1) "Top level expressions must be S-expressions")

        describe "macro definition" $ do
            it "simple macro" $ do
                let input = Ast [ListNode (TextRange 0 10)
                        [ SymbolNode (TextRange 1 9) "macrodef"
                        , SymbolNode (TextRange 10 14) "test"
                        , ListNode (TextRange 15 17) []
                        , SymbolNode (TextRange 18 19) "x"
                        ]]
                let expected = (input, [MacroInfo "test" [] (Ast [SymbolNode (TextRange 18 19) "x"])])
                analyseAst input `shouldBe` Right expected

            it "cannot redefine macrodef" $ do
                let input = Ast [ListNode (TextRange 0 10)
                        [ SymbolNode (TextRange 1 6) "defun"
                        , SymbolNode (TextRange 7 15) "macrodef"
                        , SymbolNode (TextRange 16 20) "test"
                        ]]
                analyseAst input `shouldBe` Left (SyntaxError (TextRange 1 6) "Cannot redefine macrodef")

            it "multiple macros" $ do
                let input = Ast
                        [ ListNode (TextRange 0 10)
                            [ SymbolNode (TextRange 1 9) "macrodef"
                            , SymbolNode (TextRange 10 15) "test1"
                            , ListNode (TextRange 16 18) []
                            , SymbolNode (TextRange 19 20) "x"
                            ]
                        , ListNode (TextRange 21 31)
                            [ SymbolNode (TextRange 22 30) "macrodef"
                            , SymbolNode (TextRange 31 36) "test2"
                            , ListNode (TextRange 37 39) []
                            , SymbolNode (TextRange 40 41) "y"
                            ]
                        ]
                let expected = (input,
                        [ MacroInfo "test1" [] (Ast [SymbolNode (TextRange 19 20) "x"])
                        , MacroInfo "test2" [] (Ast [SymbolNode (TextRange 40 41) "y"])
                        ])
                analyseAst input `shouldBe` Right expected

            it "macro with arguments" $ do
                let input = Ast
                        [ ListNode (TextRange 0 10)
                            [ SymbolNode (TextRange 1 9) "macrodef"
                            , SymbolNode (TextRange 10 14) "test"
                            , ListNode (TextRange 15 17)
                                [ SymbolNode (TextRange 16 17) "x"
                                ]
                            , SymbolNode (TextRange 18 19) "x"
                            ]
                        ]
                let expected = (input, [MacroInfo "test" [SymbolNode (TextRange 16 17) "x"] (Ast [SymbolNode (TextRange 18 19) "x"])])
                analyseAst input `shouldBe` Right expected

            it "macro with multiple body" $ do
                let input = Ast
                        [ ListNode (TextRange 0 10)
                            [ SymbolNode (TextRange 1 9) "macrodef"
                            , SymbolNode (TextRange 10 14) "test"
                            , ListNode (TextRange 15 17) []
                            , SymbolNode (TextRange 18 19) "x"
                            , SymbolNode (TextRange 20 21) "y"
                            ]
                        ]
                let expected = (input, [MacroInfo "test" [] (Ast [SymbolNode (TextRange 18 19) "x", SymbolNode (TextRange 20 21) "y"])])
                analyseAst input `shouldBe` Right expected

        describe "ignore not top-level macrodef" $ do
            it "macrodef in function" $ do
                let input = Ast
                        [ ListNode (TextRange 0 10)
                            [ SymbolNode (TextRange 1 5) "defun"
                            , SymbolNode (TextRange 6 10) "test"
                            , ListNode (TextRange 11 13) []
                            , ListNode (TextRange 14 24)
                                [ SymbolNode (TextRange 15 23) "macrodef"
                                , SymbolNode (TextRange 24 28) "test"
                                , ListNode (TextRange 29 31) []
                                , SymbolNode (TextRange 32 33) "x"
                                ]
                            ]
                        ]
                let expected = (input, [])
                analyseAst input `shouldBe` Right expected
