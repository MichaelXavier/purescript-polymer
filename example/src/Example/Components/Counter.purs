module Example.Components.Counter where

-------------------------------------------------------------------------------
import Control.Timer
import qualified Data.StrMap as SM
import Control.Monad.Eff.Ref
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup
-------------------------------------------------------------------------------
import Polymer.Expression
import Polymer.Markup
import Polymer.Types
-------------------------------------------------------------------------------



-------------------------------------------------------------------------------
newtype CounterState = CounterState { count :: Number }

countProp = Prop (\(CounterState s) -> s.count)


succ :: CounterState -> CounterState
succ (CounterState n) = CounterState $ n { count = n.count + 1 }


-------------------------------------------------------------------------------
proto :: Proto CounterState ( timer :: Timer )
proto = Proto { state: CounterState {count: 0}
              , methods: SM.singleton "inc" inc
              }


-------------------------------------------------------------------------------
inc :: PolymerMethod CounterState (timer :: Timer)
inc = do
  cs <- ask
  lift $ modifyRef cs succ

-------------------------------------------------------------------------------
markup :: PolymerScope CounterState Markup
markup = do
  count <- embed $ ref countProp
  return $ do
    template do
      h1 $ text $ "Count " ++ count
