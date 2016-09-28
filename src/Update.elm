module Update exposing (Msg(..), update, loadStore)

import Task
import Task
import Keyboard
import Http
import DomUtils
import Ports
import Key
import Model exposing (..)


type Msg
    = NoOp
    | StoreLoaded Store
    | StoreFaild Http.Error
    | SetCurrentAsset Asset
    | SetSearchQuery String
    | SetAssetFilter ( String, String )
    | ClearAssetFilter
    | ToggleShortcutBar
    | DownloadCurrentAsset
    | KeyPress Keyboard.KeyCode


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        StoreFaild error ->
            model ! []

        StoreLoaded store ->
            { model | store = store } ! []

        SetCurrentAsset asset ->
            { model | currentAsset = Just asset } ! []

        SetSearchQuery searchQuery ->
            { model | searchQuery = searchQuery } ! []

        SetAssetFilter ( filterKey, filterValue ) ->
            let
                newFilter =
                    makeAssetFilter filterKey filterValue
            in
                { model | assetFilter = Just newFilter } ! []

        ClearAssetFilter ->
            { model | assetFilter = Nothing } ! []

        ToggleShortcutBar ->
            toggleShortcutBar model

        DownloadCurrentAsset ->
            downloadCurrentAsset model

        KeyPress keyCode ->
            handleShortcut model keyCode


handleShortcut : Model -> Keyboard.KeyCode -> ( Model, Cmd Msg )
handleShortcut model keyCode =
    case Key.fromCode keyCode of
        Key.ShiftSlash ->
            toggleShortcutBar model

        Key.Slash ->
            focusSearchBar model

        Key.KeyD ->
            downloadCurrentAsset model

        Key.Enter ->
            copyCurrentCopypasta model

        Key.Other ->
            model ! []


focusSearchBar : Model -> ( Model, Cmd Msg )
focusSearchBar model =
    let
        command =
            DomUtils.focusSelector ".js-search-bar-input"
                |> Task.perform (\_ -> NoOp) (\_ -> NoOp)
    in
        model ! [ command ]


copyCurrentCopypasta : Model -> ( Model, Cmd Msg )
copyCurrentCopypasta model =
    let
        command =
            case model.currentAsset of
                Nothing ->
                    Cmd.none

                Just asset ->
                    DomUtils.copyToClipboard asset.copypasta
                        |> Task.perform (\_ -> NoOp) (\_ -> NoOp)
    in
        model ! [ command ]


toggleShortcutBar : Model -> ( Model, Cmd Msg )
toggleShortcutBar model =
    { model | isShortcutBarOpen = not model.isShortcutBarOpen } ! []


downloadCurrentAsset : Model -> ( Model, Cmd Msg )
downloadCurrentAsset model =
    let
        command =
            case model.currentAsset of
                Nothing ->
                    Cmd.none

                Just asset ->
                    Ports.saveFile
                        { content = asset.originalSvg
                        , name = asset.fileName
                        , mimeType = "image/svg+xml"
                        }
    in
        model ! [ command ]


makeAssetFilter : String -> String -> AssetFilter
makeAssetFilter filterKey filterValue =
    let
        getter =
            case filterKey of
                "fileDir" ->
                    .fileDir

                "baseSize" ->
                    .baseSize

                _ ->
                    (\_ -> "")
    in
        { value = filterValue
        , isIncluded = (getter >> ((==) filterValue))
        }


loadStore : Cmd Msg
loadStore =
    "/store.json"
        |> Http.get decodeStore
        |> Task.perform StoreFaild StoreLoaded
