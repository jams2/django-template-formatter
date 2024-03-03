{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Tree
  ( HtmlNode (..),
    HtmlElement (..),
    HtmlAttr (..),
    MetaNode (..),
    EitherMeta (..),
    PyVal (..),
    PyIdent,
  )
where

import Data.Default
import Data.Text qualified as T

data HtmlNode
  = ElementNode HtmlElement
  | TextNode T.Text
  deriving (Eq, Show)

data HtmlElement = HtmlElement
  { eName :: EitherMeta T.Text,
    eAttrs :: [EitherMeta HtmlAttr],
    eChildren :: [EitherMeta HtmlNode],
    eIsVoid :: Bool
  }
  deriving (Eq, Show)

instance Default HtmlElement where
  def =
    HtmlElement
      { eName = Node "",
        eAttrs = [],
        eChildren = [],
        eIsVoid = False
      }

data HtmlAttr
  = QuotedAttr T.Text T.Text
  | UnquotedAttr T.Text T.Text
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
