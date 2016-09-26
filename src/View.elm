module View exposing (view)

import Html
import Html exposing (Html, div, input, text, span, code)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (attribute, placeholder, class, classList, value)
import String
import InlineSvg exposing (inlineSvg, makeSvg)
import Model exposing (..)
import Update exposing (..)


view : Model -> Html Msg
view model =
    div [ class "c-app-container" ]
        [ viewSidebar model
        , div [ class "c-app-main" ]
            [ viewContentHeader model.searchQuery
            , div [ class "c-content" ]
                [ viewAssetList model ]
            ]
        , viewPane model.currentAsset
        ]


viewContentHeader : String -> Html Msg
viewContentHeader searchQuery =
    div [ class "c-content-header" ]
        [ div [ class "c-content-header__section" ]
            [ div [ class "c-search-bar" ]
                [ inlineSvg "search" [ attribute "class" "c-search-bar__icon" ]
                , input
                    [ class "c-search-bar__input"
                    , placeholder "Search assets..."
                    , onInput SetSearchQuery
                    , value searchQuery
                    ]
                    []
                ]
            ]
        , div [ class "c-content-header__section" ]
            [ div [ class "c-dropdown__trigger" ]
                [ div [ class "c-dropdown__trigger-inner" ]
                    [ span [] [ text "Sort by" ]
                    , inlineSvg "arrow-down"
                        [ attribute "class" "c-icon c-icon--16px c-dropdown__trigger-arrow" ]
                    ]
                ]
            ]
        ]


viewSidebar : Model -> Html Msg
viewSidebar model =
    div [ class "c-app-sidebar" ]
        [ div [ class "c-app-sidebar__logo" ]
            [ div [ class "c-dropdown__trigger" ]
                [ inlineSvg "logo" []
                , inlineSvg "arrow-down"
                    [ attribute "class" "c-icon c-icon--16px c-dropdown__trigger-arrow" ]
                ]
            ]
        , viewSidebarFilters model.assetFilter model.store.filters
        ]


viewSidebarFilters : Maybe AssetFilter -> List Filter -> Html Msg
viewSidebarFilters maybeCurrentFilter allFilters =
    let
        noneFilter =
            div [ class "c-sidebar-filter" ]
                [ div
                    [ classList
                        [ "c-sidebar-filter__item" => True
                        , "is-active" => (maybeCurrentFilter == Nothing)
                        ]
                    , onClick ClearAssetFilter
                    ]
                    [ span [ class "c-sidebar-filter__name" ]
                        [ text "All assets" ]
                    ]
                ]

        assetFilters =
            List.map (viewSidebarFilter maybeCurrentFilter) allFilters
    in
        div [] (noneFilter :: assetFilters)


viewSidebarFilter : Maybe AssetFilter -> Filter -> Html Msg
viewSidebarFilter maybeCurrentFilter filterData =
    let
        isCurrentItem =
            case maybeCurrentFilter of
                Nothing ->
                    \_ -> False

                Just currentFilter ->
                    \filterItem -> filterItem.name == currentFilter.value

        filterTitle =
            div [ class "c-sidebar-filter__title" ]
                [ text filterData.name ]

        toFilterItem item =
            div
                [ classList
                    [ "c-sidebar-filter__item" => True
                    , "is-active" => isCurrentItem item
                    ]
                , onClick (SetAssetFilter ( filterData.value, item.name ))
                ]
                [ span [ class "c-sidebar-filter__name" ]
                    [ text item.name ]
                , span [ class "c-sidebar-filter__count" ]
                    [ text (toString item.count) ]
                ]

        filterItems =
            List.map toFilterItem filterData.items
    in
        div [ class "c-sidebar-filter" ] (filterTitle :: filterItems)


containsQuery : String -> List (Asset -> String) -> Asset -> Bool
containsQuery query getters asset =
    let
        lowedQuery =
            String.toLower query

        lowedValues =
            List.map (\get -> get asset |> String.toLower) getters
    in
        List.any (String.contains lowedQuery) lowedValues


viewAssetList : Model -> Html Msg
viewAssetList model =
    let
        filteredAssets =
            case model.assetFilter of
                Nothing ->
                    model.store.assets

                Just assetFilter ->
                    List.filter assetFilter.isIncluded model.store.assets

        foundAssets =
            if model.searchQuery /= "" then
                List.filter
                    (containsQuery model.searchQuery [ .fileName, .fileDir ])
                    filteredAssets
            else
                filteredAssets
    in
        div [ class "c-asset-list" ]
            (List.map (viewAsset model.currentAsset) foundAssets)


viewAsset : Maybe Asset -> Asset -> Html Msg
viewAsset maybeCurrentAsset asset =
    let
        isActive =
            case maybeCurrentAsset of
                Nothing ->
                    False

                Just currentAsset ->
                    asset == currentAsset
    in
        div
            [ classList
                [ "c-asset-item" => True
                , "is-active" => isActive
                ]
            , onClick (SetCurrentAsset asset)
            ]
            [ makeSvg asset.svgData [] ]


viewPane : Maybe Asset -> Html Msg
viewPane maybeCurrentAsset =
    div [ class "c-app-pane" ] <|
        case maybeCurrentAsset of
            Nothing ->
                [ div [ class "c-placeholder" ]
                    [ div [ class "c-placeholder__title" ]
                        [ text "No Selection" ]
                    ]
                ]

            Just currentAsset ->
                [ div [ class "c-app-pane__section" ]
                    [ viewPanePreview currentAsset ]
                , div [ class "c-app-pane__section" ]
                    [ div [ class "c-app-pane__title" ]
                        [ text "Copypasta" ]
                    , code [ class "c-app-pane__copypasta" ]
                        [ text currentAsset.copypasta ]
                    ]
                , div [ class "c-app-pane__section" ]
                    [ div [ class "c-app-pane__title" ]
                        [ text "Details" ]
                    , viewPaneDetails currentAsset
                    ]
                , div [ class "c-app-pane__section" ]
                    [ div [ class "c-btn c-btn--tertiary c-btn--full" ]
                        [ text "Download" ]
                    ]
                ]


viewPanePreview : Asset -> Html Msg
viewPanePreview asset =
    let
        canScaleUp =
            (max asset.width asset.height) <= 50

        originalSvg =
            makeSvg asset.svgData
                [ attribute "class" "c-asset-preview__item c-asset-preview__item--base" ]
    in
        div [ class "c-asset-preview" ] <|
            if canScaleUp then
                [ makeSvg asset.svgData
                    [ attribute "class" "c-asset-preview__item"
                    , attribute "width" (toString (asset.width * 2))
                    , attribute "height" (toString (asset.height * 2))
                    ]
                , originalSvg
                ]
            else
                [ originalSvg ]


viewPaneDetails : Asset -> Html Msg
viewPaneDetails asset =
    let
        details =
            [ ( "File name", .fileName )
            , ( "Directory", .fileDir )
            , ( "Base size", .fullBaseSize )
            , ( "Original file size", .fileSize )
            , ( "Optimized file size", .optimizedFileSize )
            ]

        toDetailsItem ( name, getValue ) =
            div [ class "c-asset-details__item" ]
                [ div [ class "c-asset-details__name" ]
                    [ text name ]
                , div [ class "c-asset-details__value" ]
                    [ text (getValue asset) ]
                ]
    in
        div [ class "c-asset-details" ]
            (List.map toDetailsItem details)


{-| Convenience for making tuples in classList.
-}
(=>) : a -> b -> ( a, b )
(=>) =
    (,)
