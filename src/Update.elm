module Update exposing (Msg(..), update, loadStore)

import Task
import Http
import Model exposing (..)
import Ports


type Msg
    = NoOp
    | StoreLoaded Store
    | StoreFaild Http.Error
    | SetCurrentAsset Asset
    | SetSearchQuery String
    | SetAssetFilter ( String, String )
    | ClearAssetFilter
    | ToggleShortcutBar
    | DownloadAsset Asset


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

        SetAssetFilter ( key, value ) ->
            let
                getter =
                    case key of
                        "fileDir" ->
                            .fileDir

                        "baseSize" ->
                            .baseSize

                        _ ->
                            (\_ -> "")

                assetFilter =
                    { value = value
                    , isIncluded = (getter >> ((==) value))
                    }
            in
                { model | assetFilter = Just assetFilter } ! []

        ClearAssetFilter ->
            { model | assetFilter = Nothing } ! []

        ToggleShortcutBar ->
            { model | isShortcutBarOpen = not model.isShortcutBarOpen } ! []

        DownloadAsset asset ->
            let
                saveFile =
                    Ports.saveFile
                        { content = asset.originalSvg
                        , name = asset.fileName
                        , mimeType = "image/svg+xml"
                        }
            in
                ( model, saveFile )


loadStore : Cmd Msg
loadStore =
    "/store.json"
        |> Http.get decodeStore
        |> Task.perform StoreFaild StoreLoaded
