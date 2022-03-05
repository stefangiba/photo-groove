module Utils.Decoders exposing (..)

import Json.Decode exposing (Decoder, int, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Model.Types exposing (Photo)
import Utils.Helpers exposing (buildPhoto)


photoDecoder : Decoder Photo
photoDecoder =
    succeed buildPhoto
        |> required "url" string
        |> required "size" int
        |> optional "title" string "(untitled)"
