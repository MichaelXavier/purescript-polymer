module Polymer.Types
    ( ElementName()
    , ElementNameError(..)
    , AttributeName()
    , AttributeNameError(..)
    , PolymerElement(..)
    , PolymerElementError(..)
    , Proto(..)
    , elementNameStr
    , mkElementName
    , mkAttributeName
    , attributeNameStr
    ) where

-------------------------------------------------------------------------------
import Control.Monad.State.Trans
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
--TODO: smart constructor
newtype PolymerElement a m = PolymerElement {
      name :: ElementName
      --TODO: maybe have a separate property for template? do you
      --always want a template?
    , markup :: Markup
    , attributes :: [AttributeName]
    , proto :: Proto a m
    }


-------------------------------------------------------------------------------
newtype Proto a m = Proto {
      state :: a -- Ehhh
    , methods :: SM.StrMap (StateT a m Unit) -- need a way to resolve references to the compiled module
    }


-------------------------------------------------------------------------------
--TODO: is it practical to validate expressions?
data PolymerElementError = PEInvalidBind String
