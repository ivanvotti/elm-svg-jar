module Key exposing (Key(..), fromCode)

import Dict exposing (Dict)
import Keyboard


type Key
    = Enter
    | ShiftSlash
    | Slash
    | KeyD
    | Other


fromCode : Keyboard.KeyCode -> Key
fromCode keyCode =
    codesDict
        |> Dict.get keyCode
        |> Maybe.withDefault Other


codesDict : Dict Keyboard.KeyCode Key
codesDict =
    Dict.fromList
        [ ( 13, Enter )
        , ( 63, ShiftSlash )
        , ( 47, Slash )
        , ( 100, KeyD )
        ]
