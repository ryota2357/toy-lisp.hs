{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import           Test.Hspec
import           ToyLisp.Parser (parse)
import           ToyLisp.Syntax

spec :: Spec
spec = do
    describe "parse ok" $ do
        it "empty input" $ do
            parse "" `shouldBe` Right (Ast [])
            parse " " `shouldBe` Right (Ast [])

        describe "symbol" $ do
            it "single letter symbols" $
                parse "a" `shouldBe` Right (Ast [SymbolNode (TextRange 0 1) "a"])

            it "multiple letter symbols" $
                parse "abc" `shouldBe` Right (Ast [SymbolNode (TextRange 0 3) "abc"])

            it "symbols with numbers" $ do
                parse "a1b2c3" `shouldBe` Right (Ast [SymbolNode (TextRange 0 6) "a1b2c3"])
                parse "123abc" `shouldBe` Right (Ast [SymbolNode (TextRange 0 6) "123abc"])

            it "symbols with special characters" $ do
                parse "+-*/" `shouldBe` Right (Ast [SymbolNode (TextRange 0 4) "+-*/"])
                parse "<=>!" `shouldBe` Right (Ast [SymbolNode (TextRange 0 4) "<=>!"])

        describe "integer" $ do
            it "zero" $
                parse "0" `shouldBe` Right (Ast [IntNode (TextRange 0 1) 0])

            it "positive integers" $
                parse "42" `shouldBe` Right (Ast [IntNode (TextRange 0 2) 42])

            it "negative integers" $
                parse "-42" `shouldBe` Right (Ast [IntNode (TextRange 0 3) (-42)])

            it "integers with zero starting" $
                parse "0123" `shouldBe` Right (Ast [IntNode (TextRange 0 4) 123])

        describe "float" $ do
            it "zero" $
                parse "0.0" `shouldBe` Right (Ast [FloatNode (TextRange 0 3) 0.0])

            it "positive floats" $
                parse "42.0" `shouldBe` Right (Ast [FloatNode (TextRange 0 4) 42.0])

            it "negative floats" $
                parse "-42.0" `shouldBe` Right (Ast [FloatNode (TextRange 0 5) (-42.0)])

            it "floats with zero starting" $
                parse "0123.0" `shouldBe` Right (Ast [FloatNode (TextRange 0 6) 123.0])

            -- it "floats with no integer part" $
            --     parse ".42" `shouldBe` Right (Ast [FloatNode (TextRange 0 3) 0.42])
            --
            -- it "floats with no decimal part" $
            --     parse "42." `shouldBe` Right (Ast [FloatNode (TextRange 0 3) 42.0])

        describe "list" $ do
            it "empty list" $
                parse "()" `shouldBe` Right (Ast [ListNode (TextRange 0 2) []])

            it "single element list" $
                parse "(a)" `shouldBe`
                    Right (Ast [
                        ListNode (TextRange 0 3) [
                            SymbolNode (TextRange 1 2) "a"
                        ]
                    ])
            it "nested list" $
                parse "(a (b c))" `shouldBe`
                    Right (Ast [
                        ListNode (TextRange 0 9) [
                            SymbolNode (TextRange 1 2) "a",
                            ListNode (TextRange 3 8) [
                                SymbolNode (TextRange 4 5) "b",
                                SymbolNode (TextRange 6 7) "c"
                            ]
                        ]
                    ])

        describe "quote" $ do
            it "quotes a symbol" $
                parse "'abc" `shouldBe`
                    Right (Ast [
                        ListNode (TextRange 0 4) [
                            SymbolNode (TextRange 0 1) "quote",
                            SymbolNode (TextRange 1 4) "abc"
                        ]
                    ])

            it "nested quotes" $
                parse "'(a 1 'c)" `shouldBe`
                    Right (Ast [
                        ListNode (TextRange 0 9) [
                            SymbolNode (TextRange 0 1) "quote",
                            ListNode (TextRange 1 9) [
                                SymbolNode (TextRange 2 3) "a",
                                IntNode (TextRange 4 5) 1,
                                ListNode (TextRange 6 8) [
                                    SymbolNode (TextRange 6 7) "quote",
                                    SymbolNode (TextRange 7 8) "c"
                                ]
                            ]
                        ]
                    ])

    describe "parse error" $ do
        it "missing closing parenthesis" $
            parse "(" `shouldBe` Left (SyntaxError (TextRange 1 2) "Expected ')'")

        it "missing opening parenthesis" $
            parse ")" `shouldBe` Left (SyntaxError (TextRange 0 1) "Unexpected character")

        it "empty list with extra closing parenthesis" $
            parse "())" `shouldBe` Left (SyntaxError (TextRange 2 3) "Unexpected character")
