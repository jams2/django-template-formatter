{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserSpec (spec) where

import Parser
import Test.Hspec
import Text.Megaparsec (parseMaybe)
import Tree

spec :: Spec
spec = do
  describe "pyIdent" $ do
    let parse' = parseMaybe pyIdent
    context "with valid input" $ do
      it "parses a typical identifier" $ do
        parse' "foo" `shouldBe` Just "foo"
      it "parses an identifier with a leading underscore" $ do
        parse' "_foo" `shouldBe` Just "_foo"
      it "parses a single character identifier" $ do
        parse' "x" `shouldBe` Just "x"
      it "parses the identifier '_'" $ do
        parse' "_" `shouldBe` Just "_"
    context "with invalid input" $ do
      it "fails on a single digit" $ do
        parse' "1" `shouldBe` Nothing
      it "fails on '0'" $ do
        parse' "0" `shouldBe` Nothing
      it "fails on multiple digits" $ do
        parse' "123" `shouldBe` Nothing
      it "fails on multiple digits with a leading 0" $ do
        parse' "0123" `shouldBe` Nothing
  describe "pyVar" $ do
    let parse' = parseMaybe pyVar
    context "with valid input" $ do
      it "parses a valid variable" $ do
        parse' "foo" `shouldBe` Just (PyVar "foo")
      it "parses a valid variable with a leading underscore" $ do
        parse' "_foo" `shouldBe` Just (PyVar "_foo")
      it "parses a valid single character variable" $ do
        parse' "x" `shouldBe` Just (PyVar "x")
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
                [Node "foo ", Meta (TemplateVar (PyVar "bar"))]
            )
      it "parses a string with a meta node only" $ do
        parse' "\"{{ bar }}\""
          `shouldBe` Just
            ( QuotedValue
                DoubleQuote
                [Meta (TemplateVar (PyVar "bar"))]
            )
      it "parses a string with a meta node and a normal node" $ do
        parse' "\"{{ bar }} baz\""
          `shouldBe` Just
            ( QuotedValue
                DoubleQuote
                [Meta (TemplateVar (PyVar "bar")), Node " baz"]
            )
      it "parses whitespace around a meta node" $ do
        parse' "'    {{ bar }}    '"
          `shouldBe` Just
            ( QuotedValue
                SingleQuote
                [Node "    ", Meta (TemplateVar (PyVar "bar")), Node "    "]
            )
      it "parses multiple meta nodes" $ do
        parse' "\"{{ bar }}{{ baz }}\""
          `shouldBe` Just
            ( QuotedValue
                DoubleQuote
                [Meta (TemplateVar (PyVar "bar")), Meta (TemplateVar (PyVar "baz"))]
            )
      it "parses multiple combinations of text and meta nodes, not ended by meta" $ do
        parse' "\"foo {{ bar }} baz {{ qux }} foo\""
          `shouldBe` Just
            ( QuotedValue
                DoubleQuote
                [ Node "foo ",
                  Meta (TemplateVar (PyVar "bar")),
                  Node " baz ",
                  Meta (TemplateVar (PyVar "qux")),
                  Node " foo"
                ]
            )
      it "parses multiple combinations of text and meta nodes, ended by meta" $ do
        parse' "\"foo {{ bar }} baz {{ qux }}\""
          `shouldBe` Just
            ( QuotedValue
                DoubleQuote
                [ Node "foo ",
                  Meta (TemplateVar (PyVar "bar")),
                  Node " baz ",
                  Meta (TemplateVar (PyVar "qux"))
                ]
            )
