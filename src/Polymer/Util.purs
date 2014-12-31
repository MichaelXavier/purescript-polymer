module Polymer.Util
    ( undefined
    , selem
    ) where

-------------------------------------------------------------------------------
import qualified Data.String as S
-------------------------------------------------------------------------------


foreign import undefined :: forall a. a


selem :: String -> String -> Boolean
selem needle haystack = S.indexOf needle haystack >= 0
