-- https://html.spec.whatwg.org/multipage/syntax.html#syntax
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Parser
  ( stringLiteral,
    pyIdent,
    pyRef,
    pyVal,
    pyVar,
    pyString,
    templateVar,
    htmlVoidElement,
    metaNode,
    quotedValues,
    quotedValue,
  )
where

import Control.Applicative.Combinators qualified as C
import Data.Char (isAlpha, isAscii, isDigit, isSpace)
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

pyIndex :: Parser T.Text
pyIndex = lexeme $ takeWhile1P (Just "digit") isDigit

-- Combined parser for any string literal
stringLiteral :: Parser T.Text
stringLiteral =
  T.pack <$> lexeme (doubleQuotedString <|> singleQuotedString)
    <?> "string literal"
  where
    doubleQuotedString = char '"' *> manyTill L.charLiteral (char '"')
    singleQuotedString = char '\'' *> manyTill L.charLiteral (char '\'')

-- Python identifiers
pyIdent :: Parser PyIdent
pyIdent =
  lexeme $
    T.cons
      <$> (satisfy initialChar <?> "initial identifier char")
      <*> takeWhileP (Just "subsequent ident chars") subsequentChars
  where
    initialChar c = isAlpha c || c == '_'
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
  lexeme $
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
templateVar = TemplateVar <$> ("{{" *> hspace *> pyVal <* hspace <* "}}")

voidIdent :: Parser T.Text
voidIdent =
  lexeme $
    string' "br"
      <|> string' "hr"
      <|> string' "col"
      <|> string' "img"
      <|> string' "wbr"
      <|> string' "area"
      <|> string' "base"
      <|> string' "link"
      <|> string' "meta"
      <|> string' "embed"
      <|> string' "input"
      <|> string' "param"
      <|> string' "track"
      <|> string' "keygen"
      <|> string' "source"
      <|> string' "command"

maybeMetaAttrName :: Parser (EitherMeta T.Text)
maybeMetaAttrName =
  ((Node <$> attrName) <|> (Meta <$> metaNode))
    <?> "HTML attribute name or template node"

attrName :: Parser T.Text
attrName = lexeme $ T.pack <$> some (satisfy attrNameChar)
  where
    attrNameChar c =
      not (isSpace c)
        && c /= '"'
        && c /= '\''
        && c /= '>'
        && c /= '/'
        && c /= '='
        && isAscii c

emptyAttr :: Parser HtmlAttr
emptyAttr = EmptyAttr <$> attrName <?> "empty element attribute"

attrEq :: Parser T.Text
attrEq = lexeme $ string "="

unquotedAttr :: Parser HtmlAttr
unquotedAttr =
  ( lexeme $
      UnquotedAttr <$> name <* attrEq <*> unquotedValue
  )
    <?> "unquoted element attribute"
  where
    name = maybeMetaAttrName <?> "unquoted attribute name"
    unquotedValue =
      lexeme $
        ( ( (Node . T.pack <$> some (satisfy unquotedValueChar))
              <|> (Meta <$> metaNode)
          )
            <?> "unquoted attribute value"
        )
    unquotedValueChar c =
      not (isSpace c)
        && c /= '"'
        && c /= '\''
        && c /= '='
        && c /= '<'
        && c /= '>'
        && c /= '`'

quoteMark :: Parser QuoteType
quoteMark = string "'" *> pure SingleQuote <|> string "\"" *> pure DoubleQuote

quoteParser :: QuoteType -> Parser T.Text
quoteParser SingleQuote = string "'"
quoteParser DoubleQuote = string "\""

quotedValues :: Parser T.Text -> Parser [EitherMeta T.Text]
quotedValues delim = do
  maybeEnd <- C.optional delim
  case maybeEnd of
    Just _ -> return []
    Nothing -> do
      (chars, end) <- manyTill_ L.charLiteral (eitherMeta delim)
      return $ case chars of
        [] -> end
        toks -> Node (T.pack toks) : end
  where
    eitherMeta :: Parser a -> Parser [EitherMeta T.Text]
    eitherMeta a = pure . Meta <$> try metaNode_ <|> lookAhead a *> pure []

quotedValue :: Parser QuotedValue
quotedValue = do
  delim <- quoteMark
  values <- many $ quotedValues (quoteParser delim)
  return $ QuotedValue delim (concat values)

quotedAttr :: Parser HtmlAttr
quotedAttr =
  ( lexeme $ do
      name <- maybeMetaAttrName
      _ <- attrEq
      value <- quotedValue
      return $ QuotedAttr name value
  )
    <?> "quoted element attribute"

htmlAttr :: Parser HtmlAttr
htmlAttr =
  (try unquotedAttr <|> try quotedAttr <|> emptyAttr)
    <?> "HTML attribute"

metaNode :: Parser MetaNode
metaNode = lexeme templateVar <?> "meta node"

metaNode_ :: Parser MetaNode
metaNode_ = templateVar <?> "meta node"

elementAttrs :: Parser [EitherMeta HtmlAttr]
elementAttrs = many $ (Meta <$> metaNode) <|> (Node <$> htmlAttr)

htmlVoidElement :: Parser HtmlVoidElement
htmlVoidElement = lexeme $ do
  _ <- lexeme $ string "<"
  name <- ((Node <$> voidIdent) <|> (Meta <$> metaNode)) <?> "void element name"
  attrs <- elementAttrs <?> "void element attributes"
  _ <- lexeme $ string ">" <|> string "/>"
  return $ HtmlVoidElement {voidElementName = name, voidElementAttrs = attrs}
