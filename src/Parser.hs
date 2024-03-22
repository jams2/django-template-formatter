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
    templateFilter,
    templateVar,
    htmlVoidElement,
    metaNode,
    quotedValues,
    quotedValue,
    eitherMeta,
  )
where

import Control.Applicative.Combinators qualified as C
import Control.Applicative.Combinators.NonEmpty qualified as CNE
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

someNE :: Parser a -> Parser (NonEmpty a)
someNE = CNE.some

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

integer :: Parser T.Text
integer = lexeme $ takeWhile1P (Just "digit") isDigit

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

pyNumber :: Parser PyVal
pyNumber = PyNumber <$> integer

pyAttr :: Parser PyVal
pyAttr = pyNumber <|> PyVar <$> pyIdent

dot :: Parser Char
dot = char '.'

pyVar :: Parser PyVal
pyVar = PyVar <$> pyIdent

pyString :: Parser PyVal
pyString = PyString <$> stringLiteral

foldlNodeNe :: (a -> a -> a) -> NonEmpty a -> a
foldlNodeNe f xs = case xs of
  (x :| []) -> x
  (x :| [y]) -> f x y
  (x :| (y : ys)) -> fold' (f x y) ys
  where
    fold' acc [] = acc
    fold' acc (z : zs) = fold' (f acc z) zs

pyRef :: Parser PyVal
pyRef =
  foldlNodeNe PyGetAttr <$> do
    first <- pyVar <?> "variable reference"
    rest <- many ((dot *> pyAttr) <?> "attribute lookup")
    return $ first :| rest

templateFilter :: Parser Filter
templateFilter = do
  rand <- pyVal
  rest <- someNE suffix
  return $ foldFilters rand rest
  where
    suffix = do
      name <- string "|" *> pyIdent
      arg <- optional (string ":" *> pyVal)
      return (name, arg)
    foldFilters :: PyVal -> NonEmpty (PyIdent, Maybe PyVal) -> Filter
    foldFilters rand ((f, arg) :| xs) = fold' (Filter (Left rand) f arg) xs
    fold' :: Filter -> [(PyIdent, Maybe PyVal)] -> Filter
    fold' x [] = x
    fold' x ((f, arg) : xs) = fold' (Filter (Right x) f arg) xs

pyVal :: Parser PyVal
pyVal = pyRef <|> pyString <|> pyNumber

templateVar :: Parser MetaNode
templateVar = TemplateVar <$> ("{{" *> hspace *> node <* hspace <* "}}")
  -- TODO: implement this without backtracking
  where node = (Left <$> try templateFilter) <|> (Right <$> pyVal)

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

eitherMeta :: Parser a -> Parser (EitherMeta a)
eitherMeta p =
  Meta
    <$> try (metaNode <?> "template node")
      <|> (Node <$> p)

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

-- This doesn't need EitherMeta as the parser that calls
-- it (elementAttrs) is EitherMeta.
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
    name = eitherMeta (attrName <?> "unquoted attribute name")
    unquotedValue =
      lexeme $
        eitherMeta (concreteValue <?> "unquoted attribute value")
    concreteValue = T.pack <$> some (satisfy unquotedValueChar)
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
      (chars, end) <- manyTill_ L.charLiteral (eitherMeta' delim)
      return $ case chars of
        [] -> end
        toks -> Node (T.pack toks) : end
  where
    eitherMeta' :: Parser T.Text -> Parser [EitherMeta T.Text]
    eitherMeta' a = pure . Meta <$> try metaNode_ <|> lookAhead a *> pure []

quotedValue :: Parser QuotedValue
quotedValue = do
  delim <- quoteMark
  values <- many $ quotedValues (quoteParser delim)
  return $ QuotedValue delim (concat values)

quotedAttr :: Parser HtmlAttr
quotedAttr =
  ( lexeme $ do
      name <- eitherMeta (attrName <?> "HTML attribute name")
      _ <- attrEq
      value <- eitherMeta (quotedValue <?> "quoted attribute value")
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
elementAttrs = many $ eitherMeta htmlAttr

htmlVoidElement :: Parser HtmlVoidElement
htmlVoidElement = lexeme $ do
  _ <- lexeme $ string "<"
  name <- eitherMeta voidIdent <?> "void element name"
  attrs <- elementAttrs <?> "void element attributes"
  wasClosed <- closingTag name
  return $ HtmlVoidElement name attrs wasClosed
  where
    closingTag :: EitherMeta T.Text -> Parser Bool
    closingTag name = case name of
      Meta _ -> string "/>" *> pure True
      Node _ -> (string "/>" *> pure True) <|> (string ">" *> pure False)
