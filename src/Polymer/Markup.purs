module Polymer.Markup
    ( template
    , embed
    ) where


-------------------------------------------------------------------------------
import Text.Smolder.Markup
-------------------------------------------------------------------------------
import Polymer.Expression
import Polymer.Types
-------------------------------------------------------------------------------

-- expr :: forall a. Expression a -> Markup
-- expr e = text $ exprS e


-- --TODO: maybe an attribute combinator?
-- exprS :: forall a. Expression a -> String
-- exprS = renderExpression


template :: Markup -> Markup
template = parent "template"


-------------------------------------------------------------------------------
embed :: forall p a. Expression p a -> PolymerScope p String
embed  = return <<< renderExpression
