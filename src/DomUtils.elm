module DomUtils exposing (blurSelector, focusSelector, Selector, Error(..))

import Native.DomUtils
import Task exposing (Task)


type Error
    = NotFound String


type alias Selector =
    String


focusSelector : Selector -> Task Error ()
focusSelector =
    Native.DomUtils.focusSelector


blurSelector : Selector -> Task Error ()
blurSelector =
    Native.DomUtils.blurSelector
