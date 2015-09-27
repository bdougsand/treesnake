import Char
import Color
import Debug 
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)
import Keyboard
import Text
import Time exposing (..)

type alias Places = List (Float, Float)

type alias Snake = {
    places: Places,
    dir: {x: Float, y: Float}
}

type alias Game = {
    over: Bool,
    paused: Bool,
    frame: Int,
    snake: Snake,
    food: Places,
    -- dimensions in # of squares
    w: Int,
    h: Int

}

--scoreStyle = 
--  { Text.defaultStyle |
--    height <- Just 20,
--    color <- Color.black,
--    bold <- True}

score game =
  length game.snake.places

statusMessage game =
  if | game.over -> "Game Over"
     | True -> toString (score game)

collision place places =
  member place places

collideWalls {w, h} (x, y) =
  x < 0 || y < 0 || x >= w || y >= h


nextHead (x, y) dir =
  (x+dir.x, y+dir.y)

movePlaces places dir =
  case (head places) of
    Nothing ->
      []

    Just h ->
      (nextHead h dir) :: (take ((length places) - 1) places)

extendPlaces places dir =
  case (head places) of
    Nothing ->
      [(0, 0)]

    Just h ->
      (nextHead h dir) :: places

snakeHead {places} =
  case (head places) of
    Nothing -> (0, 0) -- this should never happen
    Just h -> h

isMoving snake =
  snake.dir /= { x = 0, y = 0}

nextSnakeHead snake dir =
  nextHead (snakeHead snake) dir

-- validTurn dir keys =
--   if keys.x /= 0 then
--     keys.x + dir.x /= 0
--   else
--     if keys.y /= 0 then
--       keys.y + dir.y /= 0

newDirection dir keys =
  if keys.x == 0 then
    if keys.y == 0 then
      dir
    else
      {x = 0, y = keys.y}
  else
    {x = keys.x, y = 0}

drawPlace size (x, y) =
  group [
    rect size size
      |> filled Color.black,
    rect size size
      |> outlined (solid Color.white)]
  |> move (x*size, y*size)

drawPlaces size places =
  places
    |> Debug.watch "places"
    |> map (drawPlace size)
    |> group

drawFood size (x, y) =
  circle (size/4)
    |> filled Color.red
    |> move (x*size, y*size)

drawFoodPieces size places =
  places
    |> Debug.watch "food"
    |> map (drawFood size)
    |> group

drawScore game =
  statusMessage game
    |> Text.fromString
    --|> Text.style scoreStyle
    |> text

updateSnake (timeDelta, keys) snake =
  let
    dir = (newDirection snake.dir keys)
  in
    { snake |
        dir <- dir,
        places <- if dir == {x=0, y=0} 
                  then snake.places
                  else (movePlaces snake.places dir)
    }

--updateGame: (Time, {x: Int, y: Int}) -> Game -> Game
updateGame (timeDelta, keys, shouldReset) game =
  if 
    | shouldReset -> board
    | game.over -> game
    | True ->
    let
    dir = (newDirection snake.dir keys)
    newHead = nextSnakeHead snake dir
    ate = (member newHead game.food)
    snake = updateSnake (timeDelta, keys) game.snake
    food = if ate 
            then filter ((/=) newHead) game.food 
            else game.food
    in
      if collideWalls game newHead ||
         member newHead snake.places then
        { game | over <- True }
        else 
        { game |
          snake <- snake,
          frame <- game.frame + 1
        }


player: Snake
player = Snake [(0, 0), (0, 1), (0, 2)] {x = 0, y = 0}

board = Game False False 0 player [(10, 10)] 25 25

squareSize = 20

view: Game -> Element
view game =
  let
    width = game.w * 20
    height = game.h * 20
  in
    collage width height [
     (outlined (solid Color.black) (rect width height)),
     (move (-220, -220) (drawScore game)),
     (move (-(width/2)+squareSize/2, -(height/2)+squareSize/2)
      (group [(drawFoodPieces 20 game.food),
              (drawPlaces 20 game.snake.places)]))]
  -- show (toString (movePlaces [(1, 2), (1, 3), (1, 4), (2, 4), (3, 4)] Up))
  -- Signal.map view input

main = Signal.map view (Signal.foldp updateGame board input)
--main = view (board player)

input =
  Signal.sampleOn delta (Signal.map3 (,,) delta Keyboard.arrows reset)

-- Reset signal
reset = Keyboard.isDown (Char.toCode 'r')

delta : Signal Time
delta =
  Signal.map(\t -> t) (fps 5)
