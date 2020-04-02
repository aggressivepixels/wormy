module Main exposing (main)

import Browser
import Browser.Events exposing (Visibility(..))
import Cell exposing (Cell(..))
import Direction exposing (Direction(..))
import Game exposing (Game, State(..))
import Json.Decode as Decode exposing (Decoder)
import View


type Msg
    = FramePassed Float
    | MoveRequested Direction
    | ChangeStateRequested
    | NewFoodCellGenerated Cell
    | VisibilityChanged Visibility


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
    ( Game.initial, Game.generateFood NewFoodCellGenerated Game.initial )


update : Msg -> Game -> ( Game, Cmd Msg )
update msg game =
    case ( msg, game.state ) of
        ( FramePassed delta, _ ) ->
            let
                newGame =
                    Game.update delta game
            in
            if Game.shouldGenerateFood newGame then
                ( newGame, Game.generateFood NewFoodCellGenerated game )

            else
                ( newGame, Cmd.none )

        ( MoveRequested direction, _ ) ->
            ( Game.updateTargetDirection direction game, Cmd.none )

        ( ChangeStateRequested, Title ) ->
            ( Game.changeState Playing game, Cmd.none )

        ( ChangeStateRequested, Playing ) ->
            ( Game.changeState Paused game, Cmd.none )

        ( ChangeStateRequested, Paused ) ->
            ( Game.changeState Playing game, Cmd.none )

        ( ChangeStateRequested, Over ) ->
            let
                newGame =
                    Game.changeState Playing Game.initial
            in
            ( newGame, Game.generateFood NewFoodCellGenerated newGame )

        ( ChangeStateRequested, Won ) ->
            let
                newGame =
                    Game.changeState Playing Game.initial
            in
            ( newGame, Game.generateFood NewFoodCellGenerated newGame )

        ( NewFoodCellGenerated food, _ ) ->
            if Game.canPlaceFood food game then
                ( Game.placeFood food game, Cmd.none )

            else
                ( game, Game.generateFood NewFoodCellGenerated game )

        ( VisibilityChanged Hidden, Playing ) ->
            ( Game.changeState Paused game, Cmd.none )

        ( _, _ ) ->
            ( game, Cmd.none )


keyDecoder : Decoder Msg
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen keyToMsg


keyToMsg : String -> Decoder Msg
keyToMsg s =
    case s of
        "ArrowLeft" ->
            Decode.succeed (MoveRequested Left)

        "ArrowRight" ->
            Decode.succeed (MoveRequested Right)

        "ArrowUp" ->
            Decode.succeed (MoveRequested Up)

        "ArrowDown" ->
            Decode.succeed (MoveRequested Down)

        " " ->
            Decode.succeed ChangeStateRequested

        _ ->
            Decode.fail ("Not interested in " ++ s)


subscriptions : Game -> Sub Msg
subscriptions game =
    case game.state of
        Playing ->
            Sub.batch
                [ Browser.Events.onAnimationFrameDelta FramePassed
                , Browser.Events.onVisibilityChange VisibilityChanged
                , Browser.Events.onKeyDown keyDecoder
                ]

        _ ->
            Browser.Events.onKeyDown keyDecoder
