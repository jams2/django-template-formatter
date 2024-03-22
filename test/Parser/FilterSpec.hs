{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.FilterSpec (spec) where

import Parser
import Test.Hspec
import Text.Megaparsec (parseMaybe)
import Tree

spec :: Spec
spec = do
  describe "templateFilter" $ do
    let parse' = parseMaybe templateFilter
    context "without arguments" $ do
      it "parses a simple filter" $ do
        parse' "foo|bar" `shouldBe` Just (Filter (Left (PyVar "foo")) "bar" Nothing)
      it "parses with a string operand" $ do
        parse' "'foo'|bar" `shouldBe` Just (Filter (Left (PyString "foo")) "bar" Nothing)
      it "parses with an int operand" $ do
        parse' "1|bar" `shouldBe` Just (Filter (Left (PyNumber "1")) "bar" Nothing)
      it "fails without an operand" $ do
        parse' "|bar" `shouldBe` Nothing
      it "fails without an operator" $ do
        parse' "foo|" `shouldBe` Nothing
      it "fails with a number as operator" $ do
        parse' "foo|1" `shouldBe` Nothing
      it "fails with a string as operator" $ do
        parse' "foo|'bar'" `shouldBe` Nothing
