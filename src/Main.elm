module Main exposing (main)

import Browser
import Browser.Events exposing (Visibility(..))
import Game exposing (Cell, Direction(..), Game, State(..))
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import View


main : Program () Game Msg
main =
    Browser.element
        { init = init
        , view = View.view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Game, Cmd Msg )
init _ =
    ( Game.initial
    , Game.generateFood NewFood Game.initial
    )


type Msg
    = Frame Float
    | Move Direction
    | ChangeState
    | NewFood Cell
    | VisibilityChange Visibility


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case ( msg, game.state ) of
        ( Frame delta, _ ) ->
            let
                newGame =
                    game
                        |> Game.update delta
            in
            if Game.shouldGenerateFood newGame then
                ( newGame
                , Game.generateFood NewFood game
                )

            else
                ( newGame
                , Cmd.none
                )

        ( Move direction, _ ) ->
            ( Game.updateTargetDirection direction game
            , Cmd.none
            )

        ( ChangeState, Title ) ->
            ( Game.changeState Playing game
            , Cmd.none
            )

        ( ChangeState, Playing ) ->
            ( Game.changeState Paused game
            , Cmd.none
            )

        ( ChangeState, Paused ) ->
            ( Game.changeState Playing game
            , Cmd.none
            )

        ( ChangeState, Over ) ->
            let
                newGame =
                    Game.changeState Playing Game.initial
            in
            ( newGame
            , Game.generateFood NewFood newGame
            )

        ( ChangeState, Won ) ->
            let
                newGame =
                    Game.changeState Playing Game.initial
            in
            ( newGame
            , Game.generateFood NewFood newGame
            )

        ( NewFood food, _ ) ->
            if game |> Game.canPlaceFood food then
                ( Game.placeFood food game
                , Cmd.none
                )

            else
                ( game
                , Game.generateFood NewFood game
                )

        ( VisibilityChange Hidden, Playing ) ->
            ( Game.changeState Paused game
            , Cmd.none
            )

        ( _, _ ) ->
            ( game
            , Cmd.none
            )


keyDecoder : Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToMsg


keyToMsg : String -> Decoder Msg
keyToMsg string =
    case string of
        "ArrowLeft" ->
            Decode.succeed (Move Left)

        "ArrowRight" ->
            Decode.succeed (Move Right)

        "ArrowUp" ->
            Decode.succeed (Move Up)

        "ArrowDown" ->
            Decode.succeed (Move Down)

        " " ->
            Decode.succeed ChangeState

        _ ->
            Decode.fail ("Not interested in " ++ string)


subscriptions : Game -> Sub Msg
subscriptions game =
    case game.state of
        Playing ->
            Sub.batch
                [ Browser.Events.onAnimationFrameDelta Frame
                , Browser.Events.onVisibilityChange VisibilityChange
                , Browser.Events.onKeyDown keyDecoder
                ]

        _ ->
            Browser.Events.onKeyDown keyDecoder
