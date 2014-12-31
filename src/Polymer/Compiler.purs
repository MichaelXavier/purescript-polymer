module Polymer.Compiler
    ( compile
    , render
    ) where

-------------------------------------------------------------------------------
import Text.Smolder.Markup
import Text.Smolder.HTML.Attributes (name)
import qualified Text.Smolder.Renderer.String as SS
-------------------------------------------------------------------------------
import Polymer.Types
import Polymer.Util
-------------------------------------------------------------------------------

render :: PolymerElement -> String
render = SS.render <<< compile

compile :: PolymerElement -> Markup
compile (PolymerElement n mu proto) = polymerElement n mu


polymerElement :: ElementName -> Markup -> Markup
polymerElement n mu = parent "polymer-element" ! name (elementNameStr n) $ mu
