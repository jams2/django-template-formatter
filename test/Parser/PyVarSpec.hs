{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.PyVarSpec (spec) where

import Parser
import Test.Hspec
import Text.Megaparsec (parseMaybe)
import Tree

spec :: Spec
spec = do
  describe "pyVar" $ do
    let parse' = parseMaybe pyVar
    context "with valid input" $ do
      it "parses a valid variable" $ do
        parse' "foo" `shouldBe` Just (PyVar "foo")
      it "parses a valid variable with a leading underscore" $ do
        parse' "_foo" `shouldBe` Just (PyVar "_foo")
      it "parses a valid single character variable" $ do
        parse' "x" `shouldBe` Just (PyVar "x")
