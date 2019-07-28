module Wormy exposing (main)

import Browser
import Browser.Events
import Game exposing (Cell, Direction(..), Game, State(..))
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import NonEmptyList exposing (NonEmptyList)
import Svg exposing (Svg)
import Svg.Attributes
import Time exposing (Posix)


main : Program () Game Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Game, Cmd Msg )
init _ =
    Game.initial
        |> withCmd (Game.generateFood NewFood Game.initial)


type Msg
    = Frame Float
    | Move Direction
    | ChangeState
    | NewFood Cell


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case msg of
        Frame delta ->
            let
                game_ =
                    game
                        |> Game.update delta
            in
            game_
                |> (if Game.shouldGenerateFood game_ then
                        withCmd (Game.generateFood NewFood game)

                    else
                        withNoCmd
                   )

        Move direction ->
            game
                |> Game.updateTargetDirection direction
                |> withNoCmd

        ChangeState ->
            case game.state of
                Title ->
                    game
                        |> Game.changeState Playing
                        |> withNoCmd

                Playing ->
                    game
                        |> Game.changeState Paused
                        |> withNoCmd

                Paused ->
                    game
                        |> Game.changeState Playing
                        |> withNoCmd

                Over ->
                    Game.initial
                        |> Game.changeState Playing
                        |> withCmd (Game.generateFood NewFood game)

        NewFood food ->
            if game |> Game.canPlaceFood food then
                game
                    |> Game.placeFood food
                    |> withNoCmd

            else
                game
                    |> withCmd (Game.generateFood NewFood game)


keyDecoder : Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToMsg


keyToMsg : String -> Decoder Msg
keyToMsg string =
    case string of
        "ArrowLeft" ->
            Decode.succeed <| Move Left

        "ArrowRight" ->
            Decode.succeed <| Move Right

        "ArrowUp" ->
            Decode.succeed <| Move Up

        "ArrowDown" ->
            Decode.succeed <| Move Down

        " " ->
            Decode.succeed <| ChangeState

        _ ->
            Decode.fail <| "Not interested in " ++ string


view : Game -> Html Msg
view game =
    let
        overlayColor =
            "#FFFFFFD5"

        cellSize =
            30

        borderWidth =
            1

        cellBorderWidth =
            0.5

        borderColor =
            "#E0E0E0"

        fieldBackground =
            "#FAFAFA"

        foodColor =
            "#F44336"

        foodBorderColor =
            "#EF9A9A"

        wormColor =
            "#808080"

        wormBorderColor =
            "#E0E0E0"

        framePadding =
            16
    in
    Html.div
        [ Html.Attributes.style "width" "fit-content"
        , Html.Attributes.style "height" "fit-content"
        , Html.Attributes.style "border-width" <|
            String.fromFloat borderWidth
                ++ "px"
        , Html.Attributes.style "border-style" "solid"
        , Html.Attributes.style "border-color" borderColor
        , Html.Attributes.style "padding" <|
            String.fromInt framePadding
                ++ "px"
        , Html.Attributes.style "font-family" "monospace"
        ]
        [ Html.div
            [ Html.Attributes.style "width" <|
                String.fromInt (game.width * cellSize)
                    ++ "px"
            , Html.Attributes.style "height" <|
                String.fromInt (game.height * cellSize)
                    ++ "px"
            , Html.Attributes.style "border-width" <|
                String.fromFloat borderWidth
                    ++ "px"
            , Html.Attributes.style "border-style" "solid"
            , Html.Attributes.style "border-color" borderColor
            , Html.Attributes.style "background" fieldBackground
            , Html.Attributes.style "margin-bottom" <|
                String.fromInt framePadding
                    ++ "px"
            , Html.Attributes.style "position" "relative"
            ]
            [ Svg.svg
                [ Svg.Attributes.width <|
                    String.fromInt (game.width * cellSize)
                , Svg.Attributes.height <|
                    String.fromInt (game.height * cellSize)
                ]
                [ case game.food of
                    Just food ->
                        fillCell cellBorderWidth foodBorderColor cellSize foodColor food

                    Nothing ->
                        Svg.text ""
                , Svg.g []
                    (List.map
                        (fillCell cellBorderWidth wormBorderColor cellSize wormColor)
                        (NonEmptyList.toList game.worm)
                    )
                ]
            , if not (game.state == Playing) then
                Html.div
                    [ Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "height" "100%"
                    , Html.Attributes.style "background" overlayColor
                    , Html.Attributes.style "position" "absolute"
                    , Html.Attributes.style "top" "0px"
                    ]
                    [ Html.div
                        [ Html.Attributes.style "padding" <|
                            String.fromInt framePadding
                                ++ "px"
                        ]
                        [ case game.state of
                            Title ->
                                Html.div
                                    []
                                    [ Html.b
                                        [ Html.Attributes.style "font-size" "4em"
                                        ]
                                        [ Html.text "Wormy" ]
                                    , Html.p
                                        []
                                        [ Html.text "Press the arrow keys to move and the space bar to pause"
                                        ]
                                    ]

                            Paused ->
                                Html.div
                                    []
                                    [ Html.b
                                        [ Html.Attributes.style "font-size" "4em"
                                        ]
                                        [ Html.text "Paused" ]
                                    , Html.p
                                        []
                                        [ Html.text "Press the space bar to resume"
                                        ]
                                    ]

                            Over ->
                                Html.div
                                    []
                                    [ Html.b
                                        [ Html.Attributes.style "font-size" "4em"
                                        ]
                                        [ Html.text "Game over!"
                                        , Html.br [] []
                                        , Html.div
                                            [ Html.Attributes.style "font-size" "0.5em"
                                            ]
                                            [ Html.text <| "Final score: " ++ String.fromInt game.score
                                            ]
                                        ]
                                    , Html.p
                                        []
                                        [ Html.text "Press the space bar to start again"
                                        ]
                                    ]

                            _ ->
                                Html.text ""
                        ]
                    ]

              else
                Html.text ""
            ]
        , Html.div
            [ Html.Attributes.style "text-align" "right"
            ]
            [ Html.text <| "Score: " ++ String.fromInt game.score
            , Html.br [] []
            , Html.text <| "Time: " ++ formatDuration (Time.millisToPosix <| round game.elapsed)
            ]
        ]


fillCell : Float -> String -> Float -> String -> Cell -> Svg a
fillCell strokeWidth strokeColor size color { x, y } =
    Svg.rect
        [ Svg.Attributes.width <| String.fromFloat size
        , Svg.Attributes.height <| String.fromFloat size
        , Svg.Attributes.fill color
        , Svg.Attributes.stroke strokeColor
        , Svg.Attributes.strokeWidth <| String.fromFloat strokeWidth
        , Svg.Attributes.x (String.fromFloat (toFloat x * size))
        , Svg.Attributes.y (String.fromFloat (toFloat y * size))
        ]
        []


subscriptions : Game -> Sub Msg
subscriptions game =
    case game.state of
        Playing ->
            Sub.batch
                [ Browser.Events.onAnimationFrameDelta Frame
                , Browser.Events.onKeyDown keyDecoder
                ]

        _ ->
            Browser.Events.onKeyDown keyDecoder



-- CMD HELPERS


withCmd : Cmd a -> b -> ( b, Cmd a )
withCmd cmd model =
    ( model, cmd )


withNoCmd : b -> ( b, Cmd a )
withNoCmd model =
    model
        |> withCmd Cmd.none



-- TIME FORMATTING


formatDuration : Posix -> String
formatDuration duration =
    let
        minutes =
            String.fromInt <| Time.toMinute Time.utc duration

        seconds =
            String.fromInt <| Time.toSecond Time.utc duration
    in
    minutes
        ++ ":"
        ++ (if String.length seconds == 1 then
                "0" ++ seconds

            else
                seconds
           )
