module Lib
    ( someFunc
    ) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

width, height, offset :: Int
width = 300
height = 300
offset = 100
-- | Data describing the state of the pong game
data PongGame = Game { ballLoc :: (Float, Float), -- ^ Pong ball (x, y ) location
                       ballVel :: (Float, Float), -- ^ velocity
                       player1 :: Float, -- ^ Left player paddle height.
                                        -- Zero is middle of the screen
                       player2 :: Float} deriving Show -- ^ Right player paddle height

initialState :: PongGame
initialState = Game {
                    ballLoc = (-10, 30),
                    ballVel = (1, -3),
                    player1 = 40,
                    player2 = -80}

window :: Display
window = InWindow "Pong" (width, height) (offset,offset)

background :: Color
background = black
radius = 10
-- main :: IO()
-- main = animate window background frame
--        where
--          frame :: Float -> Picture
--          frame seconds = render $ moveBall seconds initialState

-- | Convert a game state into a picture
render :: PongGame -- ^ The game state to render
       -> Picture -- ^ A picture of this game state.
render game =
  pictures [ball, walls, mkPaddle rose 120 $ player1 game,
           mkPaddle orange (-120) $ player2 game]
  where
    ball =uncurry translate (ballLoc game) $ color ballColor $ circleSolid radius
    ballColor = dark red
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $ color wallColor $ rectangleSolid 270 10
    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]
    paddleColor = light (light blue)
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures [
                                translate x y $ color col $ rectangleSolid 26 86,
                                translate x y $ color col $ rectangleSolid 20 80]

drawing :: Picture
drawing = pictures [
  ball,
  walls,
  mkPaddle rose 120 (-20),
  mkPaddle orange (-120) 40]
  where
    ball = translate (-10) 40  $ color ballColor $ circleSolid 10
    ballColor = dark red
    wall  :: Float -> Picture
    wall offset =
      translate 0 offset $ color wallColor $ rectangleSolid 270 10
    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150)]
    mkPaddle :: Color -> Float -> Float -> Picture
    mkPaddle col x y = pictures [
      translate x y $ color col $ rectangleSolid 26 86,
      translate x y $ color paddleColor $ rectangleSolid 20 80]

    paddleColor = light (light blue)

fps :: Int
fps = 100

someFunc :: IO ()
someFunc = simulate window background fps initialState render update

update :: ViewPort -> Float -> PongGame -> PongGame
update _ seconds = wallBounce . moveBall seconds

-- | Update the al posistion using its current velocity
moveBall ::
  Float -- ^ The number of seconds since last update
  -> PongGame -- ^ The initial game stte
  -> PongGame -- ^ A new game state with an updated ball position

moveBall seconds game =
  game {
    ballLoc = (x', y')}
  where
    -- Old location and velocities
    (x,y) = ballLoc game
    (vx, vy)= ballVel game
    --' ' | New locations
    x'= x + vx * seconds
    y' = y + vy * seconds
-- | Detect a collision with a paddle upon collisions change
-- | change the velocity of the ball to bounce it off the paddle
paddleBounce :: PongGame -> PongGame
paddleBounce game = game {
 ballVel = (vx', vy)
           }
  where
   (x,y) = ballLoc game
   (vx, vy) = ballVel game
   vx' = case paddleCollision (player1 game) (player2 game) (ballLoc game) radius of
     true -> -vx
     false -> vx

-- | Detect a colliison with one of the side walls upon collisn update the velocity of the ball to  bounce off the wall
wallBounce :: PongGame -> PongGame
wallBounce game =
  game {
    ballVel = (vx, vy')
       }
  where
    (vx,vy ) = ballVel game
    vy' =
      case wallCollision(ballLoc game) radius of
        true -> -vy
        false -> vy

type Radius = Float
type Position = (Float, Float)
paddleLength = 120
paddleCollision :: Float -> Float -> Position -> Radius -> Bool
paddleCollision p1 p2 pos radius =  leftPaddleCollision p1 pos radius || rightPaddleCollision p2 pos radius
leftPaddleCollision :: Float -> Position -> Radius -> Bool
leftPaddleCollision p1y (bx,by) radius= bx - radius <= fromIntegral width / 2 && by - p1y <= fromIntegral paddleLength / 2
rightPaddleCollision :: Float -> Position -> Radius -> Bool
rightPaddleCollision p2y (bx,by) radius= bx + radius >= fromIntegral width / 2 && by - p2y <= fromIntegral paddleLength / 2
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
                       where
                         topCollision = y - radius <= fromIntegral height / 2
                         bottomCollision = y + radius >= fromIntegral height / 2
