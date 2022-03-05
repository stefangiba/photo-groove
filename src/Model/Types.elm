module Model.Types exposing (..)


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type alias FilterOptions =
    { filters : List { name : String, amount : Float }
    , url : String
    }


type ThumbnailSize
    = Small
    | Medium
    | Large


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String
