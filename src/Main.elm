module Main exposing (..)

import Html.App as App
import Keyboard
import Model exposing (Model, initModel)
import Update exposing (Msg(..), update, loadStore)
import View exposing (view)


main : Program Never
main =
    App.program
        { init = initModel ! [ loadStore ]
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses KeyPress
