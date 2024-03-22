{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.QuotedValueSpec (spec) where

import Parser
import Test.Hspec
import Text.Megaparsec (parseMaybe)
import Tree

spec :: Spec
spec = do
  describe "quotedValue" $ do
    let parse' = parseMaybe quotedValue
    context "without meta nodes" $ do
      it "parses an empty string with double quotes" $ do
        parse' "\"\"" `shouldBe` Just (QuotedValue DoubleQuote [])
      it "parses an empty string with single quotes" $ do
        parse' "''" `shouldBe` Just (QuotedValue SingleQuote [])
      it "parses a string with double quotes" $ do
        parse' "\"foo\"" `shouldBe` Just (QuotedValue DoubleQuote [Node "foo"])
      it "parses a string with single quotes" $ do
        parse' "'foo'" `shouldBe` Just (QuotedValue SingleQuote [Node "foo"])
      it "parses a string with double quotes and an escape" $ do
        parse' "\"foo\\\"bar\""
          `shouldBe` Just (QuotedValue DoubleQuote [Node "foo\"bar"])
      it "parses a string with single quotes and an escape" $ do
        parse' "'foo\\'bar'"
          `shouldBe` Just (QuotedValue SingleQuote [Node "foo'bar"])
      it "parses single curly braces as text nodes" $ do
        parse' "'{ { bar } }'"
          `shouldBe` Just (QuotedValue SingleQuote [Node "{ { bar } }"])
      it "parses partial template vars as text nodes" $ do
        parse' "'{{ bar }'"
          `shouldBe` Just (QuotedValue SingleQuote [Node "{{ bar }"])
    context "with meta nodes" $ do
      it "parses a string with a normal and a meta node" $ do
        parse' "\"foo {{ bar }}\""
          `shouldBe` Just
            ( QuotedValue
                DoubleQuote
                [Node "foo ", Meta (tVarMeta "bar")]
            )
      it "parses a string with a meta node only" $ do
        parse' "\"{{ bar }}\""
          `shouldBe` Just
            ( QuotedValue
                DoubleQuote
                [Meta (tVarMeta "bar")]
            )
      it "parses a string with a meta node and a normal node" $ do
        parse' "\"{{ bar }} baz\""
          `shouldBe` Just
            ( QuotedValue
                DoubleQuote
                [Meta (tVarMeta "bar"), Node " baz"]
            )
      it "parses whitespace around a meta node" $ do
        parse' "'    {{ bar }}    '"
          `shouldBe` Just
            ( QuotedValue
                SingleQuote
                [Node "    ", Meta (tVarMeta "bar"), Node "    "]
            )
      it "parses multiple meta nodes" $ do
        parse' "\"{{ bar }}{{ baz }}\""
          `shouldBe` Just
            ( QuotedValue
                DoubleQuote
                [Meta (tVarMeta "bar"), Meta (tVarMeta "baz")]
            )
      it "parses multiple combinations of text and meta nodes, not ended by meta" $ do
        parse' "\"foo {{ bar }} baz {{ qux }} foo\""
          `shouldBe` Just
            ( QuotedValue
                DoubleQuote
                [ Node "foo ",
                  Meta (tVarMeta "bar"),
                  Node " baz ",
                  Meta (tVarMeta "qux"),
                  Node " foo"
                ]
            )
      it "parses multiple combinations of text and meta nodes, ended by meta" $ do
        parse' "\"foo {{ bar }} baz {{ qux }}\""
          `shouldBe` Just
            ( QuotedValue
                DoubleQuote
                [ Node "foo ",
                  Meta (tVarMeta "bar"),
                  Node " baz ",
                  Meta (tVarMeta "qux")
                ]
            )
