{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.PyIdentSpec (spec) where

import Parser
import Test.Hspec
import Text.Megaparsec (parseMaybe)

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
