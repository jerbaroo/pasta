module Pasta.Node where

import Prelude (($), (<<<))

import Pasta.Attribute (class ToAttr, DivAttr, toAttrs)
import Pasta.Component (Node(..))
import Pasta.Element (Container(..), ContainerTag(..), Element(..))
import Pasta.Listener (Listener)

text :: forall s. String -> Node s
text = NodeElement <<< ElementInner

type NodeElement   a s = ToAttr a => Array a -> Array Listener -> Array (Node s) -> Node s
type NodeElement_  a s = ToAttr a => Array a ->                   Array (Node s) -> Node s
type NodeElement__ a s = ToAttr a =>                              Array (Node s) -> Node s

nodeElement :: forall a s. ToAttr a => ContainerTag -> Array a -> Array Listener -> Array (Node s) -> Node s
nodeElement tag as ls cs = NodeElement $ ElementContainer $ Container tag (toAttrs as) cs ls

div :: forall s. NodeElement DivAttr s
div = nodeElement Div

div_ :: forall s. NodeElement_ DivAttr s
div_ as = div as []

div__ :: forall s. NodeElement__ DivAttr s
div__ = div_ []
