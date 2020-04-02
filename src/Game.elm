module Game exposing
    ( Game
    , State(..)
    , canPlaceFood
    , changeState
    , generateFood
    , initial
    , placeFood
    , shouldGenerateFood
    , update
    , updateTargetDirection
    )

import Cell exposing (Cell(..))
import Direction exposing (Direction(..))
import NonEmptyList exposing (NonEmptyList)
import Random exposing (Generator)


cellGenerator : Int -> Int -> Generator Cell
cellGenerator width height =
    Random.map2
        Cell
        (Random.int 0 (width - 1))
        (Random.int 0 (height - 1))


generateFood : (Cell -> msg) -> Game -> Cmd msg
generateFood toMsg game =
    Random.generate toMsg (cellGenerator game.width game.height)


isCellInsideField : Int -> Int -> Cell -> Bool
isCellInsideField width height (Cell x y) =
    (x >= 0 && x < width) && (y >= 0 && y < height)


type State
    = Title
    | Playing
    | Paused
    | Over
    | Won


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


changeState : State -> Game -> Game
changeState state game =
    { game | state = state }


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


update : Float -> Game -> Game
update delta game =
    case game.state of
        Playing ->
            updateHelper delta game

        _ ->
            game


updateHelper : Float -> Game -> Game
updateHelper delta game =
    let
        newGame =
            updateTime delta game
    in
    if newGame.timer <= 0 then
        moveWorm newGame

    else
        newGame


checkGameOver : Game -> Bool
checkGameOver game =
    let
        ( head, tail ) =
            NonEmptyList.uncons game.worm
    in
    List.any ((==) head) tail
        || not (isCellInsideField game.width game.height head)


checkWon : Game -> Bool
checkWon game =
    NonEmptyList.length game.worm == game.width * game.height


shouldGenerateFood : Game -> Bool
shouldGenerateFood game =
    game.food == Nothing && game.state /= Won


updateTime : Float -> Game -> Game
updateTime delta game =
    { game
        | elapsed = game.elapsed + delta
        , timer = game.timer - delta
    }


updateTargetDirection : Direction -> Game -> Game
updateTargetDirection direction game =
    case game.state of
        Playing ->
            { game | targetDirection = direction }

        _ ->
            game


moveWorm : Game -> Game
moveWorm game =
    let
        newGame =
            moveWormHelper game
    in
    newGame
        |> (if checkGameOver newGame then
                changeState Over

            else
                identity
           )
        |> (if checkWon newGame then
                changeState Won

            else
                identity
           )


moveWormHelper : Game -> Game
moveWormHelper game =
    let
        ate =
            case game.food of
                Just food ->
                    NonEmptyList.head game.worm == food

                Nothing ->
                    False

        newDirection =
            if Direction.opposite game.direction == game.targetDirection then
                game.direction

            else
                game.targetDirection

        newHead =
            Cell.move newDirection (NonEmptyList.head game.worm)

        newWorm =
            if ate then
                NonEmptyList.cons newHead game.worm

            else
                NonEmptyList.cons newHead game.worm
                    |> NonEmptyList.dropLast
    in
    { game
        | timer = game.timer + game.frameDuration
        , worm = newWorm
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
        , direction = newDirection
        , targetDirection = newDirection
    }


canPlaceFood : Cell -> Game -> Bool
canPlaceFood cell game =
    NonEmptyList.all ((/=) cell) game.worm


placeFood : Cell -> Game -> Game
placeFood cell game =
    { game | food = Just cell }
