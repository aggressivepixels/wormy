module View exposing (view)

import Cell exposing (Cell(..))
import Game exposing (Game, State(..))
import Html exposing (Html, b, br, div, p, text)
import Html.Attributes exposing (style)
import NonEmptyList
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, width, x, y)
import Time exposing (Posix)


overlayColor : String
overlayColor =
    "#FFFFFFD5"


borderColor : String
borderColor =
    "#E0E0E0"


fieldBackgroundColor : String
fieldBackgroundColor =
    "#FAFAFA"


foodColor : String
foodColor =
    "#F44336"


foodBorderColor : String
foodBorderColor =
    "#EF9A9A"


wormColor : String
wormColor =
    "#808080"


wormBorderColor : String
wormBorderColor =
    "#E0E0E0"


cellSize : number
cellSize =
    30


borderWidth : number
borderWidth =
    1


cellBorderWidth : Float
cellBorderWidth =
    0.5


fieldPadding : number
fieldPadding =
    16


view : Game -> Html msg
view game =
    div
        [ style "position" "absolute"
        , style "left" "50%"
        , style "top" "50%"
        , style "transform" "translate(-50%, -50%)"
        ]
        [ div
            [ style "width" "fit-content"
            , style "height" "fit-content"
            , style "border-width" (String.fromFloat borderWidth ++ "px")
            , style "border-style" "solid"
            , style "border-color" borderColor
            , style "padding" (String.fromInt fieldPadding ++ "px")
            , style "font-family" "monospace"
            ]
            [ div
                [ style "width" (String.fromInt (game.width * cellSize) ++ "px")
                , style "height" (String.fromInt (game.height * cellSize) ++ "px")
                , style "border-width" (String.fromFloat borderWidth ++ "px")
                , style "border-style" "solid"
                , style "border-color" borderColor
                , style "background" fieldBackgroundColor
                , style "margin-bottom" (String.fromInt fieldPadding ++ "px")
                , style "position" "relative"
                ]
                [ svg
                    [ width (String.fromInt (game.width * cellSize))
                    , height (String.fromInt (game.height * cellSize))
                    ]
                    [ case game.food of
                        Just food ->
                            fillCell cellBorderWidth foodBorderColor cellSize foodColor food

                        Nothing ->
                            Svg.text ""
                    , g []
                        (List.map
                            (fillCell cellBorderWidth wormBorderColor cellSize wormColor)
                            (NonEmptyList.toList game.worm)
                        )
                    ]
                , if not (game.state == Playing) then
                    div
                        [ style "width" "100%"
                        , style "height" "100%"
                        , style "background" overlayColor
                        , style "position" "absolute"
                        , style "top" "0px"
                        ]
                        [ div
                            [ style "padding" (String.fromInt fieldPadding ++ "px") ]
                            [ case game.state of
                                Title ->
                                    div []
                                        [ b [ style "font-size" "4em" ] [ text "Wormy" ]
                                        , p [] [ text "Press the arrow keys to move and the space bar to pause" ]
                                        ]

                                Paused ->
                                    div
                                        []
                                        [ b [ style "font-size" "4em" ] [ text "Paused" ]
                                        , p [] [ text "Press the space bar to resume" ]
                                        ]

                                Over ->
                                    div
                                        []
                                        [ b [ style "font-size" "4em" ]
                                            [ text "Game over!"
                                            , br [] []
                                            , div [ style "font-size" "0.5em" ]
                                                [ text ("Final score: " ++ String.fromInt game.score) ]
                                            ]
                                        , p [] [ text "Press the space bar to start again" ]
                                        ]

                                Won ->
                                    div
                                        []
                                        [ b [ style "font-size" "4em" ]
                                            [ text "You win!"
                                            , br [] []
                                            , div [ style "font-size" "0.5em" ]
                                                [ text ("Final score: " ++ String.fromInt game.score) ]
                                            ]
                                        , p [] [ text "Press the space bar to start again" ]
                                        ]

                                _ ->
                                    text ""
                            ]
                        ]

                  else
                    text ""
                ]
            , div
                [ style "text-align" "right" ]
                [ text ("Score: " ++ String.fromInt game.score)
                , br [] []
                , text ("Time: " ++ formatDuration game.elapsed)
                ]
            ]
        ]


fillCell : Float -> String -> Float -> String -> Cell -> Svg a
fillCell strokeSize strokeColor size color (Cell cx cy) =
    rect
        [ width (String.fromFloat size)
        , height (String.fromFloat size)
        , fill color
        , stroke strokeColor
        , strokeWidth (String.fromFloat strokeSize)
        , x (String.fromFloat (toFloat cx * size))
        , y (String.fromFloat (toFloat cy * size))
        ]
        []


formatDuration : Float -> String
formatDuration =
    round >> Time.millisToPosix >> formatPosixDuration


formatPosixDuration : Posix -> String
formatPosixDuration duration =
    let
        minutes =
            String.fromInt (Time.toMinute Time.utc duration)

        seconds =
            String.fromInt (Time.toSecond Time.utc duration)
    in
    minutes
        ++ ":"
        ++ String.padLeft 2 '0' seconds
