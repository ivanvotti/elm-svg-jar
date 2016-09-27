module Main exposing (..)

import Html.App as App
import Model exposing (Model, initModel)
import Update exposing (Msg(NoOp), update, loadStore)
import View exposing (view)
import DomUtils
import Task


main : Program Never
main =
    let
        focusSearch =
            DomUtils.focusSelector ".js-search-bar-input"
                |> Task.perform (\_ -> NoOp) (\_ -> NoOp)
    in
        App.program
            { init = initModel ! [ loadStore, focusSearch ]
            , update = update
            , view = view
            , subscriptions = \_ -> Sub.none
            }
