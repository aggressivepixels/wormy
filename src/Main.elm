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


type alias Model =
    { game : Game
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = \{ game } -> View.view game
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Game.initial
    , Game.generateFood NewFoodCellGenerated Game.initial
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { game } =
    case ( msg, game.state ) of
        ( FramePassed delta, _ ) ->
            let
                newGame =
                    game
                        |> Game.update delta
            in
            if Game.shouldGenerateFood newGame then
                ( Model newGame
                , Game.generateFood NewFoodCellGenerated game
                )

            else
                ( Model newGame
                , Cmd.none
                )

        ( MoveRequested direction, _ ) ->
            ( Model (Game.updateTargetDirection direction game)
            , Cmd.none
            )

        ( ChangeStateRequested, Title ) ->
            ( Model (Game.changeState Playing game)
            , Cmd.none
            )

        ( ChangeStateRequested, Playing ) ->
            ( Model (Game.changeState Paused game)
            , Cmd.none
            )

        ( ChangeStateRequested, Paused ) ->
            ( Model (Game.changeState Playing game)
            , Cmd.none
            )

        ( ChangeStateRequested, Over ) ->
            let
                newGame =
                    Game.changeState Playing Game.initial
            in
            ( Model newGame
            , Game.generateFood NewFoodCellGenerated newGame
            )

        ( ChangeStateRequested, Won ) ->
            let
                newGame =
                    Game.changeState Playing Game.initial
            in
            ( Model newGame
            , Game.generateFood NewFoodCellGenerated newGame
            )

        ( NewFoodCellGenerated food, _ ) ->
            if game |> Game.canPlaceFood food then
                ( Model (Game.placeFood food game)
                , Cmd.none
                )

            else
                ( Model game
                , Game.generateFood NewFoodCellGenerated game
                )

        ( VisibilityChanged Hidden, Playing ) ->
            ( Model (Game.changeState Paused game)
            , Cmd.none
            )

        ( _, _ ) ->
            ( Model game
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
            Decode.fail ("Not interested in " ++ string)


subscriptions : Model -> Sub Msg
subscriptions { game } =
    case game.state of
        Playing ->
            Sub.batch
                [ Browser.Events.onAnimationFrameDelta FramePassed
                , Browser.Events.onVisibilityChange VisibilityChanged
                , Browser.Events.onKeyDown keyDecoder
                ]

        _ ->
            Browser.Events.onKeyDown keyDecoder
