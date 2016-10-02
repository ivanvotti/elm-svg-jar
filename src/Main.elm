module Main exposing (..)

import Keyboard
import RouteUrl
import Router
import Model exposing (Model, initModel)
import Update exposing (Msg(..), update, loadStore)
import View exposing (view)


main : Program Never
main =
    RouteUrl.program
        { init = initModel ! [ loadStore ]
        , update = update
        , view = view
        , delta2url = Router.delta2hash
        , location2messages = Router.hash2messages
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses KeyPress
