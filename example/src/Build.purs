module Build
    ( main
    ) where

-------------------------------------------------------------------------------
import Control.Monad.Eff
import Control.Timer
import Data.Either
import Data.Function
import Debug.Trace
import Polymer.Compiler
import Polymer.Types
-------------------------------------------------------------------------------
import qualified Example.Components.Counter as Counter
-------------------------------------------------------------------------------

foreign import exit "var exit = require('process').exit;" :: forall e. Fn1 Number (Eff e Unit)


main = either bail (trace <<< render) mkEl
  where
    bail s = do
      trace s
      runFn1 exit 1


-------------------------------------------------------------------------------
mkEl :: forall e. Either String (PolymerElement Counter.CounterState (timer :: Timer))
mkEl = do
  n <- showErr $ mkElementName "counter" -- will explode
  return $ PolymerElement { name: n
                          , attributes: []
                          , markup: Counter.markup
                          , proto : Counter.proto
                          }


-------------------------------------------------------------------------------
showErr :: forall a b. (Show a) => Either a b -> Either String b
showErr (Left x)  = Left $ show x
showErr (Right x) = Right x
