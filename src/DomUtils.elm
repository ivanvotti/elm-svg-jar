module DomUtils
    exposing
        ( blurSelector
        , focusSelector
        , copyToClipboard
        , Selector
        , Error(..)
        )

import Native.DomUtils
import Task exposing (Task)


type Error
    = NotFound String
    | CopyFailed


type alias Selector =
    String


focusSelector : Selector -> Task Error ()
focusSelector =
    Native.DomUtils.focusSelector


blurSelector : Selector -> Task Error ()
blurSelector =
    Native.DomUtils.blurSelector


copyToClipboard : String -> Task Error ()
copyToClipboard =
    Native.DomUtils.copyToClipboard
