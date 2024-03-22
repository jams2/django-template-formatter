{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Tree
  ( HtmlNode (..),
    HtmlVoidElement (..),
    HtmlElementOpener (..),
    HtmlElementCloser (..),
    HtmlAttr (..),
    QuoteType (..),
    QuotedValue (..),
    MetaNode (..),
    EitherMeta (..),
    PyVal (..),
    PyIdent,
    Filter (..),
    tVarMeta,
    tVarFilterMeta,
  )
where

import Data.Text qualified as T

data HtmlNode
  = VoidNode HtmlVoidElement
  | OpenerNode HtmlElementOpener
  | CloserNode HtmlElementCloser
  | TextNode T.Text
  deriving (Eq, Show)

data HtmlVoidElement
  = HtmlVoidElement
      (EitherMeta T.Text) -- element name
      [EitherMeta HtmlAttr] -- attrs
      Bool -- was closed
  deriving (Eq, Show)

data HtmlElementOpener
  = HtmlElementOpener
      (EitherMeta T.Text) -- element name
      [EitherMeta HtmlAttr] -- attrs
  deriving (Eq, Show)

data HtmlElementCloser = HtmlElementCloser (EitherMeta T.Text)
  deriving (Eq, Show)

data QuoteType = SingleQuote | DoubleQuote
  deriving (Eq, Show)

data QuotedValue = QuotedValue QuoteType [EitherMeta T.Text]
  deriving (Eq, Show)

data HtmlAttr
  = QuotedAttr (EitherMeta T.Text) (EitherMeta QuotedValue)
  | UnquotedAttr (EitherMeta T.Text) (EitherMeta T.Text)
  | EmptyAttr T.Text
  deriving (Eq, Show)

data EitherMeta a
  = Meta MetaNode
  | Node a
  deriving (Eq, Show)

data MetaNode
  = TemplateTag PyIdent
  | TemplateBlock PyIdent
  | TemplateVar (Either Filter PyVal)
  | TemplateFilter Filter
  | BlockComment T.Text
  | LineComment T.Text
  deriving (Eq, Show)

tVarMeta :: PyIdent -> MetaNode
tVarMeta = TemplateVar . Right . PyVar

tVarFilterMeta :: Filter -> MetaNode
tVarFilterMeta = TemplateVar . Left

data Filter
  = Filter (Either PyVal Filter) PyIdent (Maybe PyVal)
  deriving (Eq, Show)

type PyIdent = T.Text

data PyVal
  = PyVar PyIdent
  | PyGetAttr PyVal PyVal
  | PyString T.Text
  | PyNumber T.Text
  deriving (Eq, Show)
