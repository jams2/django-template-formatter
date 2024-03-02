-- https://html.spec.whatwg.org/multipage/syntax.html#syntax
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Parser
  ( stringLiteral,
    pyRef,
    pyVal,
    pyString,
    templateVar,
  )
where

import Data.Char (isAlpha, isDigit)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Tree
import Prelude hiding (takeWhile)

type Parser = Parsec Void T.Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

data TwoOrMore a = TwoOrMore a a [a]

instance Functor TwoOrMore where
  fmap f (TwoOrMore a b rest) = TwoOrMore (f a) (f b) (fmap f rest)

isLt = (== '<')

isGt = (== '>')

sepByNE :: Parser a -> Parser b -> Parser (NonEmpty a)
sepByNE p sep = do
  first <- p
  rest <- many (sep *> p)
  return $ first :| rest

pyIndex :: Parser T.Text
pyIndex = takeWhile1P (Just "digit") isDigit

-- Combined parser for any string literal
stringLiteral :: Parser T.Text
stringLiteral =
  T.pack <$> (doubleQuotedString <|> singleQuotedString)
    <?> "string literal"
  where
    doubleQuotedString = char '"' *> manyTill L.charLiteral (char '"')
    singleQuotedString = char '\'' *> manyTill L.charLiteral (char '\'')

-- https://docs.djangoproject.com/en/5.0/ref/templates/language/#variables
pyIdent :: Parser PyIdent
pyIdent =
  T.cons
    <$> satisfy isAlpha
    <*> takeWhileP (Just "subsequent ident chars") subsequentChars
  where
    subsequentChars c = isAlpha c || isDigit c || c == '_'

pyAttr :: Parser PyVal
pyAttr = PyVar <$> (pyIndex <|> pyIdent)

dot :: Parser Char
dot = char '.'

pyVar :: Parser PyVal
pyVar = PyVar <$> pyIdent

pyString :: Parser PyVal
pyString = PyString <$> stringLiteral

pyRef :: Parser PyVal
pyRef =
  fold <$> do
    first <- pyVar <?> "variable reference"
    rest <- many ((dot *> pyAttr) <?> "attribute lookup")
    return $ first :| rest
  where
    fold :: NonEmpty PyVal -> PyVal
    fold (a :| []) = a
    fold (a :| [b]) = PyGetAttr a b
    fold (a :| (b : rest)) = fold' (PyGetAttr a b) rest
    fold' :: PyVal -> [PyVal] -> PyVal
    fold' acc [] = acc
    fold' acc (a : rest) = fold' (PyGetAttr acc a) rest

pyVal :: Parser PyVal
pyVal = pyRef <|> pyString

templateVar :: Parser MetaNode
templateVar =
  TemplateVar <$> ("{{" *> hspace *> pyVal <* hspace <* "}}")
