module Router exposing (Address, urlParser, updateModel, updateQueryParams)

import Dict exposing (Dict)
import Navigation
import Hop
import Hop.Types
import Model exposing (Model)


hopConfig : Hop.Types.Config
hopConfig =
    { hash = True
    , basePath = ""
    }


type alias Address =
    Hop.Types.Address


urlParser : Navigation.Parser ( String, Address )
urlParser =
    let
        resolver =
            Hop.makeResolver hopConfig identity
    in
        Navigation.makeParser (.href >> resolver)


updateModel : Address -> Model -> Model
updateModel routerAddress model =
    { model | routerAddress = routerAddress }
        |> applyQueryParamsToModel


updateQueryParams : Dict String String -> Model -> Cmd msg
updateQueryParams queryParams model =
    model.routerAddress
        |> Hop.setQuery queryParams
        |> Hop.output hopConfig
        |> Navigation.newUrl


qeryParamMappers : Dict String (Model -> String -> Model)
qeryParamMappers =
    Dict.fromList
        [ ( "q", \model value -> { model | searchQuery = value } )
        ]


queryParamToModel : String -> String -> Model -> Model
queryParamToModel paramName paramValue model =
    case Dict.get paramName qeryParamMappers of
        Nothing ->
            model

        Just applyParamToModel ->
            applyParamToModel model paramValue


applyQueryParamsToModel : Model -> Model
applyQueryParamsToModel model =
    Dict.foldl queryParamToModel model model.routerAddress.query
