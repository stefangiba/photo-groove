port module PhotoGroove exposing (Model, Msg(..), initialModel, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes as Attr
    exposing
        ( checked
        , class
        , classList
        , id
        , name
        , src
        , title
        , type_
        )
import Html.Events exposing (onClick)
import Http
import Json.Decode
import Json.Encode as Encode
import Model.Types exposing (..)
import Random
import Utils.CustomElements exposing (..)
import Utils.Decoders exposing (..)
import Utils.Helpers exposing (..)


main : Program Float Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { status : Status
    , activity : String
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    }


init : Float -> ( Model, Cmd Msg )
init flags =
    let
        activity =
            "Initializing Pasta v" ++ String.fromFloat flags
    in
    ( { initialModel | activity = activity }, initialCmd )


initialModel : Model
initialModel =
    { status = Loading
    , activity = ""
    , chosenSize = Medium
    , hue = 5
    , ripple = 5
    , noise = 5
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = urlPrefix ++ "photos/list.json"
        , expect = Http.expectJson GotPhotos (Json.Decode.list photoDecoder)
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    activityChanges GotActivity


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int
    | GotActivity String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSize size ->
            applyFilters { model | chosenSize = size }

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                -- Tuple.pair model <|
                --     Random.generate GotRandomPhoto <|
                --         Random.uniform firstPhoto otherPhotos
                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored _ ->
                    ( model, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )

        GotPhotos (Ok photos) ->
            case photos of
                firstPhoto :: _ ->
                    applyFilters { model | status = Loaded photos firstPhoto.url }

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( model, Cmd.none )

        SlidHue hue ->
            applyFilters { model | hue = hue }

        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlidNoise noise ->
            applyFilters { model | noise = noise }

        GotActivity activity ->
            ( { model | activity = activity }, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error: " ++ errorMessage) ]



-- VIEW HELPER FUNCTIONS


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
    [ h1 [] [ text "Photo Groove" ]
    , button [ onClick ClickedSurpriseMe ] [ text "Surprise Me!" ]
    , div [ class "activity" ] [ text model.activity ]
    , div [ class "filters" ]
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser model.chosenSize) [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
        (List.map (viewThumbnail selectedUrl)
            photos
        )
    , canvas
        [ class "large"
        , id "main-canvas"
        ]
        []
    ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , onClick (ClickedPhoto thumb.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (ClickedSize size)
            , checked (chosenSize == size)
            ]
            []
        , text (sizeToString size)
        ]


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


port setFilters : FilterOptions -> Cmd msg


port activityChanges : (String -> msg) -> Sub msg


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded _ selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { filters = filters, url = url } )

        Loading ->
            ( model, Cmd.none )

        Errored _ ->
            ( model, Cmd.none )
