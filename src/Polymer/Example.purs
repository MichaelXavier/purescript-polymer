module Polymer.Example where


-------------------------------------------------------------------------------
import Data.Identity
import Data.Either
import Text.Smolder.HTML (h1, h2, h3)
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
nestedProp = Prop (\(MyState s) -> s.nested)
stringProp = Prop (\s -> s.string)
nestedStringProp = nestedProp >>> stringProp


-------------------------------------------------------------------------------
mkEl :: forall e. Either String (PolymerElement MyState (| e))
mkEl = do
  n <- showErr $ mkElementName "my-element"
  msg <- showErr $ mkAttributeName "name"
  return $ PolymerElement { name: n
                          , attributes: [msg]
                          , markup: markup
                          , proto : myProto
                          }


myProto :: forall e. Proto MyState (| e)
myProto = Proto { state: MyState { title: "My Element", nested: { string: "ahoy"}}
                , methods: SM.empty
                }

-------------------------------------------------------------------------------
renderEl :: Either String String
renderEl = render <$> mkEl


-------------------------------------------------------------------------------
embed :: forall p a. Expression p a -> PolymerScope p String
embed  = return <<< renderExpression


-------------------------------------------------------------------------------
markup :: PolymerScope MyState Markup
markup = do
  tp <- embed $ ref titleProp
  nsp <- embed $ ref nestedStringProp
  five <- embed $ lit 5
  return $ do
    template do
      h1 $ text five
      --TODO: tie proto to PolymerElement
      h2 $ text tp
      h3 $ text nsp
