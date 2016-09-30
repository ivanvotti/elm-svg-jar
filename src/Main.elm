module Main exposing (..)

import Navigation
import Keyboard
import Model exposing (Model, initModel)
import Update exposing (Msg(..), update, urlUpdate, loadStore)
import View exposing (view)
import Router


main : Program Never
main =
    Navigation.program Router.urlParser
        { init = init
        , update = update
        , view = view
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }


init : ( String, Router.Address ) -> ( Model, Cmd Msg )
init ( _, routerAddress ) =
    Router.updateModel routerAddress initModel ! [ loadStore ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.presses KeyPress
