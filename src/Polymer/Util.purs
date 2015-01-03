module Polymer.Util
    ( undefined
    , selem
    , fmapL
    ) where

-------------------------------------------------------------------------------
import Data.Either
import qualified Data.String as S
-------------------------------------------------------------------------------


foreign import undefined :: forall a. a


selem :: String -> String -> Boolean
selem needle haystack = S.indexOf needle haystack >= 0


fmapL :: forall a b c. (a -> c) -> Either a b -> Either c b
fmapL f (Left x) = Left (f x)
fmapL _ (Right x) = Right x
