import Char
import Color
import Debug
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (..)
import Keyboard
import Random exposing (..)
import Text exposing (defaultStyle)
import Time exposing (..)

type alias Places = List (Float, Float)

type alias Snake = {
    places: Places,
    dir: {x: Float, y: Float}
}



zeroVector = { x = 0, y = 0 }

scoreStyle =
  { defaultStyle |
    height <- Just 20,
    color <- Color.black,
    bold <- True}

statusStyle =
  { scoreStyle |
    height <- Just 40 }

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
     | game.paused -> "Paused"
     | otherwise -> ""

scoreString game =
  toString (score game)

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
    |> map (drawFood size)
    |> group

drawScore game =
  scoreString game
    |> Text.fromString
    |> Text.style scoreStyle
    |> Text.monospace
    |> text

drawStatus game =
  let
    message = statusMessage game
                |> Debug.watch "status"
                |> Text.fromString
                |> Text.style statusStyle
  in
  group [
    text message,
    outlinedText (solid Color.white) message
  ]

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

updateFood game snake =
  let
    (newPlace, seed') = randPlace game.w game.h game.seed (\p -> not (member p snake.places))
  in
    {game|
      food <- case (head snake.places) of
                Nothing -> game.food
                Just place ->
                  newPlace :: (filter ((/=) place) game.food),
      seed <- seed'}

updateInterval game =
  let
    s = score game
  in
    if 
      | s < 10 -> 200
      | s < 20 -> 150
      | s < 30 -> 100
      | s < 35 -> 66
      | otherwise -> 50


incFrame game =
  { game | frame <- game.frame + 1}

setPaused isPaused game =
  { game | paused <- isPaused }

shouldUpdate game =
  game.timeSinceUpdate >= (updateInterval game)

--updateGame: (Time, {x: Int, y: Int}) -> Game -> Game
updateGame (timeDelta, keys, shouldReset, paused) game =
  if
    | shouldReset -> board
    | game.over -> game
    | paused -> { game | paused <- True }
    | shouldUpdate game ->
    let
      oldSnake = game.snake
      dir = validatedTurn oldSnake.dir keys
      newHead = nextSnakeHead oldSnake dir
      ate = member newHead game.food
      newSnake = updateSnake dir ate oldSnake
      newGame = if ate then updateFood game newSnake else game
    in
      if collideWalls game newHead ||
         selfCollide newSnake.places then
        { game | over <- True }
        else
        { newGame |
          snake <- newSnake,
          frame <- game.frame + 1,
          timeSinceUpdate <- 0,
          paused <- False
        }
    | otherwise -> 
      { game | 
        timeSinceUpdate <- game.timeSinceUpdate + (inMilliseconds timeDelta) }


player: Snake
player = Snake [(10, 10), (10, 11), (10, 12)] {x = 1, y = 0}
type alias Game = {
    over: Bool,
    paused: Bool,
    frame: Int,
    timeSinceUpdate: Float,
    snake: Snake,
    food: Places,
    -- dimensions in # of squares
    w: Int,
    h: Int,
    seed: Seed

}
board = { over= False,
          paused = False,
          frame = 0,
          timeSinceUpdate = 0,
          snake = player,
          food = [(10, 10)],
          w = 25,
          h = 25,
          seed = (initialSeed 23) }

squareSize: Float
squareSize = 30

view: Game -> Element
view game =
  let
    width = (toFloat game.w) * squareSize
    height = (toFloat game.h) * squareSize
  in
    collage (floor width) (floor height) [
     (outlined (solid Color.black) (rect width height)),
     (move (-(width/2) + 30, -(height/2) + 30) (drawScore game)),
     (move (-(width/2)+squareSize/2, 
            -(height/2)+squareSize/2)
      (group [(drawFoodPieces squareSize game.food),
              (drawPlaces squareSize game.snake.places)])),
     (drawStatus game)]


main = Signal.map view (Signal.foldp updateGame board input)

input =
  Signal.sampleOn delta (Signal.map4 (,,,) delta direction reset paused)

-- Reset signal
reset = Keyboard.isDown (Char.toCode 'R')

paused = toggler (Char.toCode ' ')

-- Generate a true signal when the space bar is first pressed. 
-- Generate false after the key is released then pressed again
toggler keyCode =
  Signal.filterMap (\(_, current, last) -> if current /= last then 
                                              Just current
                                           else
                                              Nothing)
  False
  (Signal.foldp (\isDown (last, status, lastStatus) ->
    if isDown && not last then
      (isDown, not status, status)
    else
      (isDown, status, status))
    (False, False, False)
    (Keyboard.isDown keyCode))

-- Signal that produces the last direction pressed
direction =
  Signal.foldp (\(dir, isPaused) oldDir -> 
                if dir /= zeroVector && not isPaused then dir else oldDir) 
                zeroVector
                (Signal.map2 (,) Keyboard.arrows paused)

delta : Signal Time
delta =
  Signal.map(\t -> t) (fps 20)
