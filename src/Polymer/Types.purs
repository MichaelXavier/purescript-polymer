module Polymer.Types where

-------------------------------------------------------------------------------
import Text.Smolder.Markup
-------------------------------------------------------------------------------

--TODO: smart constructor, guarantee dash present
newtype ElementName = ElementName String

--TODO: smart constructor
data PolymerElement = PolymerElement ElementName Markup PolymerProto

data PolymerProto = PolymerProto
