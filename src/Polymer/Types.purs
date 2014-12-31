module Polymer.Types
    ( ElementName()
    , ElementNameError(..)
    , PolymerElement(..)
    , PolymerProto(..)
    , elementNameStr
    , mkElementName
    ) where

-------------------------------------------------------------------------------
import Data.Either
import qualified Data.String as S
import Text.Smolder.Markup
-------------------------------------------------------------------------------
import Polymer.Util
-------------------------------------------------------------------------------


data ElementNameError = ENBlank
                      | ENMissingDash


newtype ElementName = ElementName String

elementNameStr :: ElementName -> String
elementNameStr (ElementName s) = s

mkElementName :: String -> Either ElementNameError ElementName
mkElementName s
    | S.null s    = Left ENBlank
    | selem "-" s = Right $ ElementName s
    | otherwise   = Left ENMissingDash

--TODO: smart constructor
data PolymerElement = PolymerElement ElementName Markup PolymerProto

data PolymerProto = PolymerProto
