module Wormy exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode exposing (Decoder)
import Random exposing (Generator)
import Set exposing (Set)
import Svg exposing (Svg)
import Svg.Attributes
import Time exposing (Posix)


type alias Cell =
    { x : Int
    , y : Int
    }


cellIn : Int -> Int -> Generator Cell
cellIn width height =
    Random.map2
        Cell
        (Random.int 0 (width - 1))
        (Random.int 0 (height - 1))


inField : Int -> Int -> Cell -> Bool
inField width height { x, y } =
    (x >= 0 && x < width) && (y >= 0 && y < height)


newFood : Int -> Int -> Cmd Msg
newFood width height =
    Random.generate NewFood (cellIn width height)


moveCell : Direction -> Cell -> Cell
moveCell direction { x, y } =
    case direction of
        Up ->
            Cell x (y - 1)

        Down ->
            Cell x (y + 1)

        Left ->
            Cell (x - 1) y

        Right ->
            Cell (x + 1) y


type Direction
    = Up
    | Down
    | Left
    | Right


keyDecoder : Decoder Msg
keyDecoder =
    Decode.map keyToMsg (Decode.field "key" Decode.string)


keyToMsg : String -> Msg
keyToMsg string =
    case string of
        "ArrowLeft" ->
            Move Left

        "ArrowRight" ->
            Move Right

        "ArrowUp" ->
            Move Up

        "ArrowDown" ->
            Move Down

        " " ->
            ChangeState

        _ ->
            None


oppositeDirection : Direction -> Direction
oppositeDirection direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


type State
    = Title
    | Playing
    | Paused
    | Over


type alias Model =
    { width : Int
    , height : Int
    , score : Int
    , elapsed : Float
    , timer : Float
    , fps : Float
    , wormHead : Cell
    , wormTail : List Cell
    , food : Maybe Cell
    , direction : Direction
    , newDirection : Maybe Direction
    , state : State
    }


initialModel : Model
initialModel =
    { width = 20
    , height = 10
    , score = 0
    , elapsed = 0
    , timer = 0
    , fps = 1000 / 10
    , wormHead = Cell 4 1
    , wormTail = [ Cell 3 1, Cell 2 1, Cell 1 1 ]
    , food = Nothing
    , direction = Right
    , newDirection = Nothing
    , state = Title
    }


type Msg
    = Frame Float
    | Move Direction
    | ChangeState
    | NewFood Cell
    | None


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    initialModel
        |> withCmd (newFood initialModel.width initialModel.height)


view : Model -> Html Msg
view model =
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
                String.fromInt (model.width * cellSize)
                    ++ "px"
            , Html.Attributes.style "height" <|
                String.fromInt (model.height * cellSize)
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
                    String.fromInt (model.width * cellSize)
                , Svg.Attributes.height <|
                    String.fromInt (model.height * cellSize)
                ]
                [ case model.food of
                    Just food ->
                        fillCell cellBorderWidth foodBorderColor foodColor cellSize food

                    Nothing ->
                        Svg.text ""
                , Svg.g []
                    (List.map
                        (fillCell cellBorderWidth wormBorderColor wormColor cellSize)
                        (model.wormHead :: model.wormTail)
                    )
                ]
            , if not (model.state == Playing) then
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
                        [ case model.state of
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
                                            [ Html.text <| "Final score: " ++ String.fromInt model.score
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
            [ Html.text <| "Score: " ++ String.fromInt model.score
            , Html.br [] []
            , Html.text <| "Time: " ++ formatDuration (Time.millisToPosix <| round model.elapsed)
            ]
        ]


fillCell : Float -> String -> String -> Float -> Cell -> Svg a
fillCell strokeWidth strokeColor color size { x, y } =
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Frame delta ->
            if model.state == Playing then
                let
                    model_ =
                        model
                            |> updateTime delta
                            |> slither
                in
                if
                    List.any ((==) model_.wormHead) model_.wormTail
                        || not (inField model_.width model_.height model_.wormHead)
                then
                    { model_ | state = Over }
                        |> withNoCmd

                else
                    model_
                        |> (if model_.food == Nothing then
                                withCmd (newFood model.width model.height)

                            else
                                withNoCmd
                           )

            else
                model
                    |> withNoCmd

        Move direction ->
            if model.state == Playing then
                { model
                    | newDirection = Just direction
                }
                    |> withNoCmd

            else
                model
                    |> withNoCmd

        ChangeState ->
            case model.state of
                Title ->
                    { model
                        | state = Playing
                    }
                        |> withNoCmd

                Playing ->
                    { model
                        | state = Paused
                    }
                        |> withNoCmd

                Paused ->
                    { model
                        | state = Playing
                    }
                        |> withNoCmd

                Over ->
                    { initialModel
                        | state = Playing
                    }
                        |> withCmd (newFood model.width model.height)

        NewFood food ->
            if model.wormHead == food || List.any ((==) food) model.wormTail then
                model
                    |> withCmd (newFood model.width model.height)

            else
                { model
                    | food = Just food
                }
                    |> withNoCmd

        None ->
            model
                |> withNoCmd


updateTime : Float -> Model -> Model
updateTime delta model =
    { model
        | elapsed = model.elapsed + delta
        , timer = model.timer - delta
    }


slither : Model -> Model
slither model =
    if model.timer <= 0 then
        let
            ate =
                case model.food of
                    Just food ->
                        model.wormHead == food

                    Nothing ->
                        False

            direction_ =
                case model.newDirection of
                    Just newDirection ->
                        if
                            oppositeDirection model.direction
                                == newDirection
                        then
                            model.direction

                        else
                            newDirection

                    Nothing ->
                        model.direction
        in
        { model
            | timer = model.timer + model.fps
            , wormHead = moveCell direction_ model.wormHead
            , wormTail =
                model.wormHead
                    :: (if ate then
                            model.wormTail

                        else
                            listInit model.wormTail
                       )
            , food =
                if ate then
                    Nothing

                else
                    model.food
            , score =
                if ate then
                    model.score + 1

                else
                    model.score
            , direction = direction_
            , newDirection = Nothing
        }

    else
        model


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
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



-- LIST HELPERS


listInit : List a -> List a
listInit list =
    case List.reverse list of
        [] ->
            []

        _ :: rest ->
            List.reverse rest



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
