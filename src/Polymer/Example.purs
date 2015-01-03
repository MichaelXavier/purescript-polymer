module Polymer.Example where


-------------------------------------------------------------------------------
import Data.Either
import Text.Smolder.HTML
import Text.Smolder.Markup
-------------------------------------------------------------------------------
import Polymer.Compiler
import Polymer.Expression
import Polymer.Markup
import Polymer.Types
import Polymer.Util
-------------------------------------------------------------------------------


showErr :: forall a b. (Show a) => Either a b -> Either String b
showErr = fmapL show


-------------------------------------------------------------------------------
mkEl :: Either String PolymerElement
mkEl = do
  n <- showErr $ mkElementName "my-element"
  msg <- showErr $ mkAttributeName "name"
  return $ PolymerElement { name: n , attributes: [msg] , markup: markup}


renderEl :: Either String String
renderEl = render <$> mkEl


markup :: Markup
markup = do
  template do
    h1 $ expr $ lit 5


-- five :: Expression Number
-- five = lit 5
