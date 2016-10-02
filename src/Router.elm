module Router exposing (delta2hash, hash2messages)

import String
import Navigation exposing (Location)
import RouteUrl exposing (UrlChange)
import RouteUrl.Builder as Builder exposing (Builder)
import Update exposing (Msg(..))
import Model exposing (Model, AssetFilter)


type alias QueryDeserializer =
    ( String, String -> Msg )


type alias QuerySerializer =
    ( String, Model -> String )


hash2messages : Location -> List Msg
hash2messages location =
    let
        builder =
            Builder.fromHash location.href
    in
        queryDeserializers
            |> List.map (applyDeserializer builder)
            |> List.filter (\msg -> msg /= NoOp)


delta2hash : Model -> Model -> Maybe UrlChange
delta2hash previous current =
    querySerializers
        |> List.foldl (applySerializer current) Builder.builder
        |> Builder.toHashChange
        |> Just


applyDeserializer : Builder -> QueryDeserializer -> Msg
applyDeserializer builder ( queryName, msgGen ) =
    case Builder.getQuery queryName builder of
        Nothing ->
            NoOp

        Just queryValue ->
            msgGen queryValue


queryDeserializers : List QueryDeserializer
queryDeserializers =
    [ ( "q", SetSearchQuery )
    , ( "filterBy", deserializeAssetFilter )
    ]


deserializeAssetFilter : String -> Msg
deserializeAssetFilter filterQuery =
    case String.split ":" filterQuery of
        [ filterName, filterValue ] ->
            SetAssetFilter ( filterName, filterValue )

        _ ->
            NoOp


applySerializer : Model -> QuerySerializer -> Builder -> Builder
applySerializer model ( queryName, getter ) builder =
    Builder.insertQuery queryName (getter model) builder


querySerializers : List QuerySerializer
querySerializers =
    [ ( "q", .searchQuery )
    , ( "filterBy", .assetFilter >> serializeAssetFilter )
    ]


serializeAssetFilter : Maybe AssetFilter -> String
serializeAssetFilter assetFilter =
    case assetFilter of
        Nothing ->
            ""

        Just ( filterName, filterValue ) ->
            filterName ++ ":" ++ filterValue
