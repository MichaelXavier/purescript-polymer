module Polymer.Example where


-------------------------------------------------------------------------------
import Data.Identity
import Data.Either
import Text.Smolder.HTML
import Text.Smolder.Markup
import qualified Data.StrMap as SM
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
newtype MyState = MyState {
      title :: String
    , nested :: { string :: String}
    }

titleProp = Prop (\(MyState s) -> s.title)
nestedStringProp = Prop (\(MyState s) -> s.nested.string)


-------------------------------------------------------------------------------
mkEl :: Either String (PolymerElement MyState Identity)
mkEl = do
  n <- showErr $ mkElementName "my-element"
  msg <- showErr $ mkAttributeName "name"
  return $ PolymerElement { name: n
                          , attributes: [msg]
                          , markup: markup
                          , proto : myProto
                          }


myProto :: Proto MyState Identity
myProto = Proto { state: MyState { title: "My Element", nested: { string: "ahoy"}}
                , methods: SM.empty
                }

-------------------------------------------------------------------------------
renderEl :: Either String String
renderEl = render <$> mkEl


-------------------------------------------------------------------------------
markup :: Markup
markup = do
  template do
    h1 $ expr $ lit 5
    --TODO: tie proto to PolymerElement
    h2 $ expr $ ref titleProp
    h3 $ expr $ ref nestedStringProp
