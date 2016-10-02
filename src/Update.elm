module Update exposing (Msg(..), update, loadStore)

import String
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
    | FetchStoreDone Store
    | FetchStoreFail Http.Error
    | SetCurrentAsset Asset
    | SetSearchQuery String
    | SetAssetFilter ( String, String )
    | ClearAssetFilter
    | ToggleShortcutBar
    | DownloadCurrentAsset
    | KeyPress Keyboard.KeyCode
    | ToggleIsInputInFocus


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        FetchStoreFail error ->
            model ! []

        FetchStoreDone store ->
            updateFilteredAssets { model | store = store }
                ! []

        SetSearchQuery searchQuery ->
            updateFilteredAssets { model | searchQuery = searchQuery }
                ! []

        SetAssetFilter assetFilter ->
            updateFilteredAssets { model | assetFilter = Just assetFilter }
                ! []

        ClearAssetFilter ->
            updateFilteredAssets { model | assetFilter = Nothing }
                ! []

        SetCurrentAsset asset ->
            { model | currentAsset = Just asset } ! []

        ToggleShortcutBar ->
            toggleShortcutBar model

        DownloadCurrentAsset ->
            downloadCurrentAsset model

        ToggleIsInputInFocus ->
            { model | isSearchInputInFocus = not model.isSearchInputInFocus }
                ! []

        KeyPress keyCode ->
            case model.isSearchInputInFocus of
                True ->
                    model ! []

                False ->
                    handleShortcut model keyCode


containsQuery : String -> List (Asset -> String) -> Asset -> Bool
containsQuery query getters asset =
    let
        lowedQuery =
            String.toLower query

        lowedValues =
            List.map (\get -> get asset |> String.toLower) getters
    in
        List.any (String.contains lowedQuery) lowedValues


updateFilteredAssets : Model -> Model
updateFilteredAssets model =
    let
        allAssets =
            model.store.assets

        afterFilter =
            case model.assetFilter of
                Nothing ->
                    allAssets

                Just currentFilter ->
                    allAssets
                        |> List.filter (makeAssetFilter currentFilter)

        afterSearch =
            if model.searchQuery /= "" then
                List.filter
                    (containsQuery model.searchQuery [ .fileName, .fileDir ])
                    afterFilter
            else
                afterFilter

        filteredAssets =
            afterSearch
    in
        { model | filteredAssets = filteredAssets }


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
                |> Task.perform alwaysNoOp alwaysNoOp
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
                        |> Task.perform alwaysNoOp alwaysNoOp
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


makeAssetFilter : AssetFilter -> Asset -> Bool
makeAssetFilter ( filterKey, filterValue ) =
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
        (getter >> ((==) filterValue))


loadStore : Cmd Msg
loadStore =
    "/store.json"
        |> Http.get decodeStore
        |> Task.perform FetchStoreFail FetchStoreDone


alwaysNoOp : a -> Msg
alwaysNoOp =
    always NoOp
