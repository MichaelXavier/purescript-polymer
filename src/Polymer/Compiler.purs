module Polymer.Compiler
    ( compile
    , render
    ) where

-------------------------------------------------------------------------------
import qualified Data.String as S
import Text.Smolder.Markup
import Text.Smolder.HTML.Attributes (name)
import qualified Text.Smolder.Renderer.String as SS
-------------------------------------------------------------------------------
import Polymer.Types
import Polymer.Util
-------------------------------------------------------------------------------

render :: forall a m. PolymerElement a m -> String
render = SS.prettyPrint options <<< compile
  where
    options = SS.PPOptions { indentStr: "  "}

compile :: forall a m. PolymerElement a m -> Markup
compile (PolymerElement pe) =
  polymerElement ! name' pe.name ! attributes pe.attributes $ pe.markup

polymerElement :: Markup -> Markup
polymerElement = parent "polymer-element"

name' :: ElementName -> Attribute
name' = attribute "name" <<< elementNameStr

attributes :: [AttributeName] -> Attribute
attributes ans = attribute "attributes" anStr
  where
    anStr = S.joinWith " " $ attributeNameStr <$> ans
