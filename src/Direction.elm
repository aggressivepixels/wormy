module Direction exposing (Direction(..), opposite)


type Direction
    = Up
    | Down
    | Left
    | Right


opposite : Direction -> Direction
opposite d =
    case d of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left
