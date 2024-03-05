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
  )
where

import Data.Text qualified as T

data HtmlNode
  = VoidNode HtmlVoidElement
  | OpenerNode HtmlElementOpener
  | CloserNode HtmlElementCloser
  | TextNode T.Text
  deriving (Eq, Show)

data HtmlVoidElement = HtmlVoidElement
  { voidElementName :: EitherMeta T.Text,
    voidElementAttrs :: [EitherMeta HtmlAttr]
  }
  deriving (Eq, Show)

data HtmlElementOpener = HtmlElementOpener
  { openerName :: EitherMeta T.Text,
    openerAttrs :: [EitherMeta HtmlAttr]
  }
  deriving (Eq, Show)

data HtmlElementCloser = HtmlElementCloser
  { closerName :: EitherMeta T.Text,
    closerAttrs :: [EitherMeta HtmlAttr]
  }
  deriving (Eq, Show)

data QuoteType = SingleQuote | DoubleQuote
  deriving (Eq, Show)

data QuotedValue = QuotedValue QuoteType [EitherMeta T.Text]
  deriving (Eq, Show)

data HtmlAttr
  = QuotedAttr (EitherMeta T.Text) QuotedValue
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
  | TemplateVar PyVal
  | TemplateFilter PyVal PyVal (Maybe PyVal)
  | BlockComment T.Text
  | LineComment T.Text
  deriving (Eq, Show)

type PyIdent = T.Text

data PyVal
  = PyVar PyIdent
  | PyGetAttr PyVal PyVal
  | PyString T.Text
  deriving (Eq, Show)
