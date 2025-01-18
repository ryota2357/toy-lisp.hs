{-# LANGUAGE OverloadedStrings   #-}

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

            -- it "symbols with numbers" $ do
            --     parse "a1b2c3" `shouldBe` Right (Ast [SymbolNode (TextRange 0 5) "a1b2c3"])
                -- parse "123abc" `shouldBe` Right (Ast [SymbolNode (TextRange 0 6) "123abc"])

        describe "integer" $ do
            it "positive integers" $
                parse "42" `shouldBe` Right (Ast [IntNode (TextRange 0 2) 42])

            it "zero starting integers" $
                parse "0123" `shouldBe` Right (Ast [IntNode (TextRange 0 4) 123])

            -- it "negative integers" $
            --     parse "-42" `shouldBe` Right (Ast [IntNode (TextRange 0 3) (-42)])

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
            parse "(" `shouldBe` Left (TextSize 1)

        it "missing opening parenthesis" $
            parse ")" `shouldBe` Left (TextSize 0)

        it "empty list with extra closing parenthesis" $
            parse "())" `shouldBe` Left (TextSize 2)
