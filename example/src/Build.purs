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

foreign import exit "var exit = process.exit;" :: forall e. Fn1 Number (Eff e Unit)

foreign import mkdirp """
  var mdp = require('mkdirp').sync;
  function mkdirp(p) {
    return function() {
      mdp(p);
      return {};
    }
  }
""" :: forall e. Fn1 String (Eff e Unit)

foreign import writeFileSync """
  var wfs = require('fs').writeFileSync;
  function writeFileSync(p, s) {
    return function() {
      wfs(p, s)
      return {};
    }
  }
""" :: forall e. Fn2 String String (Eff e Unit)


main = either bail (write <<< render) mkEl
  where
    write s = do
      runFn1 mkdirp "components"
      runFn2 writeFileSync "components/x-counter.html" s
    bail s = do
      trace s
      runFn1 exit 1


-------------------------------------------------------------------------------
mkEl :: forall e. Either String (PolymerElement Counter.CounterState (timer :: Timer))
mkEl = do
  n <- showErr $ mkElementName "x-counter" -- will explode
  return $ PolymerElement { name: n
                          , attributes: []
                          , markup: Counter.markup
                          , proto : Counter.proto
                          }


-------------------------------------------------------------------------------
showErr :: forall a b. (Show a) => Either a b -> Either String b
showErr (Left x)  = Left $ show x
showErr (Right x) = Right x
