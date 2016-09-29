module Model
    exposing
        ( Model
        , Asset
        , Store
        , Filter
        , AssetFilter
        , initModel
        , decodeStore
        )

import Json.Decode as D
import Json.Decode.Pipeline as DP


type alias Model =
    { store : Store
    , currentAsset : Maybe Asset
    , filteredAssets : List Asset
    , searchQuery : String
    , assetFilter : Maybe AssetFilter
    , isShortcutBarOpen : Bool
    , isSearchInputInFocus : Bool
    }


initModel : Model
initModel =
    { store = emptyStore
    , currentAsset = Nothing
    , filteredAssets = []
    , searchQuery = ""
    , assetFilter = Nothing
    , isShortcutBarOpen = False
    , isSearchInputInFocus = False
    }


emptyStore : Store
emptyStore =
    Store [] [] []


type alias AssetFilter =
    { value : String
    , isIncluded : Asset -> Bool
    }


type alias Store =
    { assets : List Asset
    , filters : List Filter
    , links : List Link
    }


type alias Asset =
    { svgData : SvgData
    , originalSvg : String
    , width : Int
    , height : Int
    , fileName : String
    , fileDir : String
    , fileSize : String
    , optimizedFileSize : String
    , baseSize : String
    , fullBaseSize : String
    , copypasta : String
    , strategy : String
    }


type alias SvgData =
    { content : String
    , attrs : List NameValue
    }


type alias NameValue =
    { name : String
    , value : String
    }


type alias Link =
    { text : String
    , url : String
    }


type alias Filter =
    { name : String
    , value : String
    , items : List FilterItem
    }


type alias FilterItem =
    { name : String
    , count : Int
    }


decodeStore : D.Decoder Store
decodeStore =
    DP.decode Store
        |> DP.required "assets" (D.list decodeAsset)
        |> DP.required "filters" (D.list decodeFilter)
        |> DP.required "links" (D.list decodeLink)


decodeAsset : D.Decoder Asset
decodeAsset =
    DP.decode Asset
        |> DP.required "svg" decodeSvg
        |> DP.required "originalSvg" D.string
        |> DP.required "width" D.int
        |> DP.required "height" D.int
        |> DP.required "fileName" D.string
        |> DP.required "fileDir" D.string
        |> DP.required "fileSize" D.string
        |> DP.required "optimizedFileSize" D.string
        |> DP.required "baseSize" D.string
        |> DP.required "fullBaseSize" D.string
        |> DP.required "copypasta" D.string
        |> DP.required "strategy" D.string


decodeSvg : D.Decoder SvgData
decodeSvg =
    DP.decode SvgData
        |> DP.required "content" D.string
        |> DP.required "attrs" (D.list decodeNameValue)


decodeNameValue : D.Decoder NameValue
decodeNameValue =
    DP.decode NameValue
        |> DP.required "name" D.string
        |> DP.required "value" D.string


decodeFilter : D.Decoder Filter
decodeFilter =
    DP.decode Filter
        |> DP.required "name" D.string
        |> DP.required "value" D.string
        |> DP.required "items" (D.list decodeFilterItem)


decodeFilterItem : D.Decoder FilterItem
decodeFilterItem =
    DP.decode FilterItem
        |> DP.required "name" D.string
        |> DP.required "count" D.int


decodeLink : D.Decoder Link
decodeLink =
    DP.decode Link
        |> DP.required "text" D.string
        |> DP.required "url" D.string
