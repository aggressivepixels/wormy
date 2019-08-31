module Wormy exposing (main)

import Browser
import Browser.Events exposing (Visibility(..))
import Game exposing (Cell, Direction(..), Game, State(..))
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Renderer


main : Program () Game Msg
main =
    Browser.element
        { init = init
        , view = Renderer.render
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
    | VisibilityChange Visibility


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case ( msg, game.state ) of
        ( Frame delta, _ ) ->
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

        ( Move direction, _ ) ->
            game
                |> Game.updateTargetDirection direction
                |> withNoCmd

        ( ChangeState, Title ) ->
            game
                |> Game.changeState Playing
                |> withNoCmd

        ( ChangeState, Playing ) ->
            game
                |> Game.changeState Paused
                |> withNoCmd

        ( ChangeState, Paused ) ->
            game
                |> Game.changeState Playing
                |> withNoCmd

        ( ChangeState, Over ) ->
            Game.initial
                |> Game.changeState Playing
                |> withCmd (Game.generateFood NewFood game)

        ( ChangeState, Won ) ->
            Game.initial
                |> Game.changeState Playing
                |> withCmd (Game.generateFood NewFood game)

        ( NewFood food, _ ) ->
            if game |> Game.canPlaceFood food then
                game
                    |> Game.placeFood food
                    |> withNoCmd

            else
                game
                    |> withCmd (Game.generateFood NewFood game)

        ( VisibilityChange visibility, Playing ) ->
            if visibility == Hidden then
                game
                    |> Game.changeState Paused
                    |> withNoCmd

            else
                game
                    |> withNoCmd

        ( _, _ ) ->
            game
                |> withNoCmd


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



-- CMD HELPERS


withCmd : Cmd a -> b -> ( b, Cmd a )
withCmd cmd model =
    ( model, cmd )


withNoCmd : b -> ( b, Cmd a )
withNoCmd model =
    model
        |> withCmd Cmd.none
