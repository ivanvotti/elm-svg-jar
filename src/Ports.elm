port module Ports exposing (..)


type alias File =
    { content : String
    , name : String
    , mimeType : String
    }


port saveFile : File -> Cmd msg
