{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}

module Tree
  ( HtmlNode (..),
    Element (..),
    Attr (..),
    MetaNode (..),
    MaybeMeta (..),
  )
where

import Data.Text qualified as T

data HtmlNode
  = ElementNode Element
  | TextNode T.Text
  deriving (Eq, Show)

data Element = Element
  { eName :: MaybeMeta T.Text,
    eAttrs :: [MaybeMeta Attr],
    eChildren :: [MaybeMeta HtmlNode],
    eIsVoid :: Bool
  }
  deriving (Eq, Show)

data Attr = Attr T.Text T.Text
  deriving (Eq, Show)

data MaybeMeta a
  = Meta MetaNode
  | Node a
  deriving (Eq, Show)

data MetaNode
  = --          name
    TemplateTag T.Text
  | --            name
    TemplateBlock T.Text
  | --          contents
    TemplateVar T.Text
  | BlockComment T.Text
  | LineComment T.Text
  deriving (Eq, Show)
