module Polymer.Runtime
    ( initElement
    , PolymerInit(..)
    ) where


-------------------------------------------------------------------------------
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Reader.Trans
import Data.Function
import qualified Data.StrMap as SM
-------------------------------------------------------------------------------
import Polymer.Types
import Polymer.Util
-------------------------------------------------------------------------------

foreign import data PolymerInit :: !


initElement :: forall s e e2. PolymerElement s e -> Eff ( polymerInit :: PolymerInit | e2) Unit
initElement (PolymerElement p) =
  runFn5 initElementP newRef
                      (mkFn2 runReaderT)
                      (elementNameStr p.name)
                      (protoState p.proto)
                      undefined
                      -- (protoMethods p.proto)


foreign import initElementP """
  function(newRef, runReaderT, name, initState, methods) {
    return function() {
      //TODO: deep copy
      var proto = initState;
      var protoRef = newRef(proto)();
      for (var k in methods) {
        proto[k] = runReaderT(methods[k], protoRef);
      }
      Polymer(name, proto);
    }
  }
""" :: forall s e e2 r m a. Fn5 (s -> Eff (ref :: Ref | e2) (RefVal s))
                                (Fn2 (ReaderT r m a) r (Eff e2 a))
                                String
                                s
                                (SM.StrMap (PolymerMethod s e))
                                (Eff (polymerInit :: PolymerInit | e2) Unit)
