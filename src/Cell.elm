module Cell exposing
    ( Cell(..)
    , move
    )

import Direction exposing (Direction(..))


type Cell
    = Cell Int Int


move : Direction -> Cell -> Cell
move d (Cell x y) =
    case d of
        Up ->
            Cell x (y - 1)

        Down ->
            Cell x (y + 1)

        Left ->
            Cell (x - 1) y

        Right ->
            Cell (x + 1) y
