module Main exposing (main)

import Browser
import Browser.Events exposing (Visibility(..))
import Cell exposing (Cell(..))
import Direction exposing (Direction(..))
import Game exposing (Game, State(..))
import Json.Decode as Decode exposing (Decoder)
import View


type Msg
    = Frame Float
    | Move Direction
    | ChangeState
    | NewFood Cell
    | VisibilityChange Visibility


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
    , Game.generateFood NewFood Game.initial
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { game } =
    case ( msg, game.state ) of
        ( Frame delta, _ ) ->
            let
                newGame =
                    game
                        |> Game.update delta
            in
            if Game.shouldGenerateFood newGame then
                ( Model newGame
                , Game.generateFood NewFood game
                )

            else
                ( Model newGame
                , Cmd.none
                )

        ( Move direction, _ ) ->
            ( Model (Game.updateTargetDirection direction game)
            , Cmd.none
            )

        ( ChangeState, Title ) ->
            ( Model (Game.changeState Playing game)
            , Cmd.none
            )

        ( ChangeState, Playing ) ->
            ( Model (Game.changeState Paused game)
            , Cmd.none
            )

        ( ChangeState, Paused ) ->
            ( Model (Game.changeState Playing game)
            , Cmd.none
            )

        ( ChangeState, Over ) ->
            let
                newGame =
                    Game.changeState Playing Game.initial
            in
            ( Model newGame
            , Game.generateFood NewFood newGame
            )

        ( ChangeState, Won ) ->
            let
                newGame =
                    Game.changeState Playing Game.initial
            in
            ( Model newGame
            , Game.generateFood NewFood newGame
            )

        ( NewFood food, _ ) ->
            if game |> Game.canPlaceFood food then
                ( Model (Game.placeFood food game)
                , Cmd.none
                )

            else
                ( Model game
                , Game.generateFood NewFood game
                )

        ( VisibilityChange Hidden, Playing ) ->
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


subscriptions : Model -> Sub Msg
subscriptions { game } =
    case game.state of
        Playing ->
            Sub.batch
                [ Browser.Events.onAnimationFrameDelta Frame
                , Browser.Events.onVisibilityChange VisibilityChange
                , Browser.Events.onKeyDown keyDecoder
                ]

        _ ->
            Browser.Events.onKeyDown keyDecoder
