module Polymer.Markup
    ( template
    ) where


-------------------------------------------------------------------------------
import Text.Smolder.Markup
-------------------------------------------------------------------------------
import Polymer.Expression
-------------------------------------------------------------------------------

-- expr :: forall a. Expression a -> Markup
-- expr e = text $ exprS e


-- --TODO: maybe an attribute combinator?
-- exprS :: forall a. Expression a -> String
-- exprS = renderExpression


template :: Markup -> Markup
template = parent "template"
