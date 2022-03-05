module Utils.CustomElements exposing (..)

import Html exposing (Attribute, Html, node)


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children
