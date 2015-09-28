import Char
import Color
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)
import Keyboard
import Random exposing (..)
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
    h: Int,
    seed: Seed

}

--scoreStyle =
--  { Text.defaultStyle |
--    height <- Just 20,
--    color <- Color.black,
--    bold <- True}

randPlace w h seed guard =
  let 
    (x, seed') = generate (int 0 (w - 1)) seed
    (y, seed'') = generate (int 0 (h - 1)) seed'
    place = ((toFloat x), (toFloat y))
  in
    if guard place then
      (place, seed'')
    else
      randPlace w h seed'' guard

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

validTurn dir keys =
 if keys.x /= 0 then
   keys.x + dir.x /= 0
 else
   if keys.y /= 0 then
     keys.y + dir.y /= 0
   else
     True

turn dir keys =
  if keys.x == 0 then
    if keys.y == 0 then
      dir
    else
      {x = 0, y = keys.y}
  else
    {x = keys.x, y = 0}

validatedTurn dir keys =
  if validTurn dir keys then
    turn dir keys
  else
    dir

drawPlace size (x, y) =
  group [
    rect size size
      |> filled Color.black,
    rect size size
      |> outlined (solid Color.white)]
  |> move (x*size, y*size)

drawPlaces size places =
  places
    --|> Debug.watch "places"
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

selfCollide places =
  let
    h = head places
    t = tail places
  in
    case h of
      Nothing -> False
      Just front ->
        case t of
          Nothing -> False
          Just rest ->
            member front rest

updateSnake dir ate snake =
  {snake|
    dir <- dir,
    places <- if ate then
                extendPlaces snake.places dir
              else
                movePlaces snake.places dir}

updateFood game place =
  let
    (newPlace, seed') = randPlace game.w game.h game.seed (\p -> not (member p game.food))
  in
    {game|
      food <- newPlace :: (filter ((/=) place) game.food),
      seed <- seed'}

--updateGame: (Time, {x: Int, y: Int}) -> Game -> Game
updateGame (timeDelta, keys, shouldReset) game =
  if
    | shouldReset -> board
    | game.over -> game
    --| game.snake.dir == {x:0, y:0} -> game
    | True ->
    let
      oldSnake = game.snake
      dir = validatedTurn oldSnake.dir keys
      newHead = nextSnakeHead oldSnake dir
      ate = member newHead game.food
      newSnake = updateSnake dir ate oldSnake
      newGame = if ate then updateFood game newHead else game
    in
      if collideWalls game newHead ||
         selfCollide newSnake.places then
        { game | over <- True }
        else
        { newGame |
          snake <- newSnake,
          frame <- game.frame + 1
        }


player: Snake
player = Snake [(10, 10), (10, 11), (10, 12)] {x = 1, y = 0}

board = Game False False 0 player [(10, 10)] 25 25 (initialSeed 23)

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
reset = Keyboard.isDown (Char.toCode 'R')

delta : Signal Time
delta =
  Signal.map(\t -> t) (fps 10)
