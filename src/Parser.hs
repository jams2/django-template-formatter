-- https://html.spec.whatwg.org/multipage/syntax.html#syntax
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Parser (templateVar) where

import Control.Applicative ((<|>))
import Data.Attoparsec.Text qualified as P
import Data.Char (isAlpha, isDigit)
import Data.Text qualified as T
import Tree qualified

isLt = (== '<')

isGt = (== '>')

maybeSelfClosing :: P.Parser Bool
maybeSelfClosing =
  P.choice
    [ return True <$> "/>",
      return False <$> P.skip isGt
    ]

-- https://docs.djangoproject.com/en/5.0/ref/templates/language/#variables
varName :: P.Parser T.Text
varName =
  P.choice
    [ T.cons
        <$> P.satisfy isAlpha
        <*> P.takeWhile subsequentChars,
      T.cons
        <$> P.satisfy isDigit
        <*> P.takeWhile1 subsequentChars
    ]
  where
    subsequentChars c = isAlpha c || isDigit c || c == '_'

horizontalSpace = P.many1' $ P.satisfy (P.inClass " \t")

templateVar :: P.Parser Tree.MetaNode
templateVar =
  Tree.TemplateVar
    <$> ("{{" *> horizontalSpace *> varName <* horizontalSpace <* "}}")
