{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser.HtmlVoidElementSpec (spec) where

import Parser
import Test.Hspec
import Text.Megaparsec (parseMaybe)
import Tree

spec :: Spec
spec = do
  describe "htmlVoidElement" $ do
    let parse' = parseMaybe htmlVoidElement
    context "without attrs" $ do
      it "parses a self-closing element" $ do
        parse' "<br  />" `shouldBe` Just (HtmlVoidElement (Node "br") [] True)
      it "parses a self-closing element without spaces" $ do
        parse' "<br/>" `shouldBe` Just (HtmlVoidElement (Node "br") [] True)
      it "parses a self-closing element with a newline" $ do
        parse' "<br\n/>" `shouldBe` Just (HtmlVoidElement (Node "br") [] True)
      it "parses a self-closing element with a newline before the name" $ do
        parse' "<\nbr/>" `shouldBe` Just (HtmlVoidElement (Node "br") [] True)
      it "parses a self-closing element with multiple newlines" $ do
        parse' "<\nbr\n\n\n/>" `shouldBe` Just (HtmlVoidElement (Node "br") [] True)
    context "with attrs" $ do
      it "parses an element with an empty attribute" $ do
        parse' "<input disabled  />"
          `shouldBe` Just
            (HtmlVoidElement (Node "input") [Node $ EmptyAttr "disabled"] True)
    context "with a var in the name position" $ do
      it "parses a self-closing element" $ do
        parse' "<{{ foo }}/>"
          `shouldBe` Just
            (HtmlVoidElement (Meta (tVarMeta "foo")) [] True)
      it "parses a self-closing element/with spaces/without attrs" $ do
        parse' "< {{ foo }}  />"
          `shouldBe` Just
            (HtmlVoidElement (Meta (tVarMeta "foo")) [] True)
