module Game exposing
    ( Cell
    , Direction(..)
    , Game
    , State(..)
    , canPlaceFood
    , generateFood
    , initial
    , isCellInsideField
    , maybeMoveWorm
    , placeFood
    , updateTargetDirection
    , updateTime
    , withState
    )

import NonEmptyList exposing (NonEmptyList)
import Random exposing (Generator)


type Direction
    = Up
    | Down
    | Left
    | Right


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


type alias Cell =
    { x : Int
    , y : Int
    }


cellGenerator : Int -> Int -> Generator Cell
cellGenerator width height =
    Random.map2
        Cell
        (Random.int 0 (width - 1))
        (Random.int 0 (height - 1))


generateFood : (Cell -> msg) -> Game -> Cmd msg
generateFood msg { width, height } =
    Random.generate msg (cellGenerator width height)


isCellInsideField : Int -> Int -> Cell -> Bool
isCellInsideField width height { x, y } =
    (x >= 0 && x < width) && (y >= 0 && y < height)


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


type State
    = Title
    | Playing
    | Paused
    | Over


type alias Game =
    { width : Int
    , height : Int
    , score : Int
    , elapsed : Float
    , timer : Float
    , frameDuration : Float
    , worm : NonEmptyList Cell
    , food : Maybe Cell
    , direction : Direction
    , targetDirection : Direction
    , state : State
    }


withState : State -> Game -> Game
withState state game =
    { game
        | state = state
    }


initial : Game
initial =
    { width = 20
    , height = 10
    , score = 0
    , elapsed = 0
    , timer = 0
    , frameDuration = 1000 / 10
    , worm = NonEmptyList.from (Cell 4 1) [ Cell 3 1, Cell 2 1, Cell 1 1 ]
    , food = Nothing
    , direction = Right
    , targetDirection = Right
    , state = Title
    }


updateTime : Float -> Game -> Game
updateTime delta game =
    { game
        | elapsed = game.elapsed + delta
        , timer = game.timer - delta
    }


updateTargetDirection : Direction -> Game -> Game
updateTargetDirection direction game =
    { game
        | targetDirection = direction
    }


maybeMoveWorm : Game -> Game
maybeMoveWorm game =
    if game.timer <= 0 then
        let
            ate =
                case game.food of
                    Just food ->
                        NonEmptyList.head game.worm == food

                    Nothing ->
                        False

            direction_ =
                if
                    oppositeDirection game.direction
                        == game.targetDirection
                then
                    game.direction

                else
                    game.targetDirection

            head_ =
                moveCell direction_ (NonEmptyList.head game.worm)

            worm_ =
                if ate then
                    NonEmptyList.cons head_ game.worm

                else
                    game.worm
                        |> NonEmptyList.cons head_
                        |> NonEmptyList.dropLast
        in
        { game
            | timer = game.timer + game.frameDuration
            , worm = worm_
            , food =
                if ate then
                    Nothing

                else
                    game.food
            , score =
                if ate then
                    game.score + 1

                else
                    game.score
            , direction = direction_
            , targetDirection = direction_
        }

    else
        game


canPlaceFood : Cell -> Game -> Bool
canPlaceFood cell game =
    NonEmptyList.all ((/=) cell) game.worm


placeFood : Cell -> Game -> Game
placeFood cell game =
    { game
        | food = Just cell
    }
