module Polymer.Types
    ( ElementName()
    , ElementNameError(..)
    , AttributeName()
    , AttributeNameError(..)
    , PolymerElement(..)
    , PolymerElementError(..)
    , Proto(..)
    , protoState
    , protoMethods
    , PolymerScope()
    , PolymerMethod(..)
    , unPolymerScope
    , elementNameStr
    , mkElementName
    , mkAttributeName
    , attributeNameStr
    ) where

-------------------------------------------------------------------------------
import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Reader.Trans
import Data.Either
import qualified Data.String as S
import qualified Data.StrMap as SM
import Text.Smolder.Markup
-------------------------------------------------------------------------------
import Polymer.Util
-------------------------------------------------------------------------------


data ElementNameError = ENBlank
                      | ENMissingDash

instance showElementNameError :: Show ElementNameError where
  show ENBlank = "ENBlank"
  show ENMissingDash = "ENMissingDash"


-------------------------------------------------------------------------------
newtype ElementName = ElementName String


-------------------------------------------------------------------------------
elementNameStr :: ElementName -> String
elementNameStr (ElementName s) = s


-------------------------------------------------------------------------------
mkElementName :: String -> Either ElementNameError ElementName
mkElementName s
    | S.null s    = Left ENBlank
    | selem "-" s = Right $ ElementName s
    | otherwise   = Left ENMissingDash


-------------------------------------------------------------------------------
newtype AttributeName = AttributeName String


-------------------------------------------------------------------------------
attributeNameStr :: AttributeName -> String
attributeNameStr (AttributeName s) = s


-------------------------------------------------------------------------------
mkAttributeName :: String -> Either AttributeNameError AttributeName
mkAttributeName s
    | S.null s  = Left ANBlank
    | otherwise = Right $ AttributeName s


-------------------------------------------------------------------------------
data AttributeNameError = ANBlank

instance showAttributeNameError :: Show AttributeNameError where
  show ANBlank = "ANBlank"


-------------------------------------------------------------------------------
--TODO: monad instance, don't export constructor. also this is a
--really silly instance. is there a point to this? Maybe it makes sense if we hide all the rendering functions
newtype PolymerScope proto a = PolymerScope a

instance functorPolymerScope :: Functor (PolymerScope p) where
  (<$>) f (PolymerScope x) = PolymerScope (f x)

instance applyPolymerScope :: Apply (PolymerScope p) where
  (<*>) (PolymerScope f) (PolymerScope a) = PolymerScope (f a)

instance bindPolymerScope :: Bind (PolymerScope p) where
  (>>=) (PolymerScope a) f = f a

instance applicativePolymerScope :: Applicative (PolymerScope p) where
  pure = PolymerScope

instance monadPolymerScope :: Monad (PolymerScope p)

unPolymerScope (PolymerScope a) = a

-------------------------------------------------------------------------------
--TODO: smart constructor
newtype PolymerElement st e = PolymerElement {
      name :: ElementName
      --TODO: maybe have a separate property for template? do you
      --always want a template?
    , markup :: PolymerScope st Markup
    , attributes :: [AttributeName] --TODO: cross ref attributes with proto
    , proto :: Proto st e
    }


type PolymerMethod st e = ReaderT (RefVal st) (Eff (ref :: Ref | e)) Unit


-------------------------------------------------------------------------------
newtype Proto st e = Proto {
      state :: st -- Ehhh
    , methods :: SM.StrMap (PolymerMethod st e)
    -- , methods :: SM.StrMap (ReaderT (RefVal st) (Eff e) Unit)
      -- TODO need a way to resolve references to the compiled module
      -- methods need to also not share keys with state keys
      -- also can't take arguments
    }

protoState (Proto p) = p.state
protoMethods (Proto p) = p.methods


-------------------------------------------------------------------------------
--TODO: is it practical to validate expressions?
data PolymerElementError = PEInvalidBind String
