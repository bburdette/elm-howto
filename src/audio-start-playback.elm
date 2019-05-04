-- This code file demonstrates how to control starting playback of audio in the update function of the elm program.
-- Source URL for the Audio file and playback volume can be configured from the UI.
-- Since its pure elm and does not rely on ports or native, it also works on http://elm-lang.org/try (tested with chrome and edge).


module Main exposing (Model, Msg(..), PlaybackParams, init, main, playbackView, startPlayback, update, updatePlaybackParams, view, withVolume)

import Browser
import Html exposing (audio, button, div, input, source, table, text)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Encode


type alias PlaybackParams =
    { sourceUrl : String
    , volume : Float
    , playbackRate : Float
    }


type alias Model =
    { selectedPlaybackParams : PlaybackParams
    , listPlayback : List PlaybackParams
    }


type Msg
    = SelectAudioSourceUrl String
    | SelectPlaybackVolume Float
    | SelectPlaybackRate Float
    | StartPlayback


init : Model
init =
    { selectedPlaybackParams =
        { sourceUrl = "http://dict.leo.org/media/audio/ZXOapx_FyRojukaMRHKS_w.mp3"
        , volume = 0.7
        , playbackRate = 1.0
        }
    , listPlayback = []
    }


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


playbackView : PlaybackParams -> Html.Html msg
playbackView playbackParams =
    audio
        [ controls False
        , autoplay True
        , property "volume" (Json.Encode.string (String.fromFloat playbackParams.volume))
        , property "playbackRate" (Json.Encode.string (String.fromFloat playbackParams.playbackRate))
        , property "mozPreservesPitch" (Json.Encode.bool False)
        , property "webkitPreservesPitch" (Json.Encode.bool False)

        -- , property "preservesPitch" (Json.Encode.bool False)
        ]
        [ source [ src playbackParams.sourceUrl ] [] ]


view : Model -> Html.Html Msg
view model =
    let
        selectedPlaybackParams =
            model.selectedPlaybackParams

        selectedSourceUrl =
            selectedPlaybackParams.sourceUrl

        selectedPlaybackVolume =
            selectedPlaybackParams.volume

        selectedPlaybackRate =
            selectedPlaybackParams.playbackRate
    in
    div []
        [ div []
            [ text "configure audio parameters"
            , div [ style "margin" "10px" ]
                [ div [] [ text "url to audio file" ]
                , div [] [ input [ placeholder "url to audio file", onInput SelectAudioSourceUrl, value selectedSourceUrl ] [] ]
                , div [] [ text "-" ]
                , div [] [ text "playback volume" ]
                , div []
                    [ input
                        [ type_ "range"
                        , HA.min "0"
                        , HA.max "100"
                        , value <| String.fromFloat (selectedPlaybackVolume * 100)
                        , onInput (String.toFloat >> Maybe.withDefault 40 >> (*) 0.01 >> SelectPlaybackVolume)
                        ]
                        []
                    , text <| (((selectedPlaybackVolume * 100) |> round |> String.fromInt) ++ "%")
                    ]
                , div [] [ text "-" ]
                , div [] [ text "playback rate " ]
                , div []
                    [ input
                        [ type_ "range"
                        , HA.min "0"
                        , HA.max "200"
                        , value <| String.fromFloat (selectedPlaybackRate * 100)
                        , onInput (String.toFloat >> Maybe.withDefault 100 >> (*) 0.01 >> SelectPlaybackRate)
                        ]
                        []
                    , text <| (((selectedPlaybackRate * 100) |> round |> String.fromInt) ++ "%")
                    ]
                ]
            , button [ onClick StartPlayback ] [ text "click to start playback" ]
            ]
        , div [] [ text "-" ]
        , div [] [ text (String.fromInt (model.listPlayback |> List.length) ++ " playbacks started") ]
        , div [] (model.listPlayback |> List.map playbackView)
        ]


startPlayback : Model -> Model
startPlayback model =
    { model | listPlayback = [ model.listPlayback, [ model.selectedPlaybackParams ] ] |> List.concat }


updatePlaybackParams : Msg -> PlaybackParams -> PlaybackParams
updatePlaybackParams msg playbackParams =
    case msg of
        SelectAudioSourceUrl url ->
            { playbackParams | sourceUrl = url }

        SelectPlaybackVolume volume ->
            { playbackParams | volume = volume }

        SelectPlaybackRate rate ->
            { playbackParams | playbackRate = rate }

        _ ->
            playbackParams


update : Msg -> Model -> Model
update msg model =
    let
        withPlaybackParams =
            { model | selectedPlaybackParams = updatePlaybackParams msg model.selectedPlaybackParams }
    in
    case msg of
        StartPlayback ->
            startPlayback withPlaybackParams

        SelectPlaybackRate rate ->
            { withPlaybackParams | listPlayback = model.listPlayback |> List.map (withRate rate) }

        SelectPlaybackVolume volume ->
            { withPlaybackParams | listPlayback = model.listPlayback |> List.map (withVolume volume) }

        _ ->
            withPlaybackParams


withVolume : Float -> PlaybackParams -> PlaybackParams
withVolume volume paramsBefore =
    { paramsBefore | volume = volume }


withRate : Float -> PlaybackParams -> PlaybackParams
withRate rate paramsBefore =
    { paramsBefore | playbackRate = rate }
