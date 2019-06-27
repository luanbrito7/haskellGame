import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random

main :: IO ()
main = play window background fps initialState render handleKeys update
    where
        update :: Float -> MyGame -> MyGame
        update secs = generateBullets . checkEnd . bulletsDismiss . moveBullets secs
        window = InWindow "Game" (400, 400) (0, 0) 
        background = black

fps :: Int
fps = 60

handleKeys :: Event -> MyGame -> MyGame
handleKeys (EventKey (Char 'a') _ _ _) game = 
  if xP1 < -200 then
    game { p1Position = (200, yP1) }
    else game { p1Position = (xP1 - 10, yP1) }
  where
    (xP1,yP1) = p1Position game
handleKeys (EventKey (Char 'd') _ _ _) game = 
  if xP1 > 200 then
    game { p1Position = (-200, yP1) }
    else game { p1Position = (xP1 + 10,yP1) }
  where
    (xP1,yP1) = p1Position game
handleKeys (EventKey (Char 'w') _ _ _) game =
  if yP1 < 0 then
    game { p1Position = (xP1,yP1 + 10) }
    else game { p1Position = (xP1, 0)}
  where
    (xP1,yP1) = p1Position game
handleKeys (EventKey (Char 's') _ _ _) game =
  if yP1 > -200 then
    game { p1Position = (xP1,yP1 - 10) }
    else game { p1Position = (xP1, -200) }
  where
    (xP1,yP1) = p1Position game
handleKeys (EventKey (Char 'q') _ _ _) game = game { bulletsTopLoc = bulletsTopLoc' ++ [(xP1, yP1 + 10)] }
  where
    (xP1,yP1) = p1Position game
    bulletsTopLoc' = bulletsTopLoc game
handleKeys (EventKey (Char 'j') _ _ _) game =
  if xP2 < -200 then
    game { p2Position = (200, yP2) }
    else 
      game { p2Position = (xP2 - 10,yP2) }
  where
    (xP2,yP2) = p2Position game
handleKeys (EventKey (Char 'l') _ _ _) game =
  if xP2 < 200 then
    game { p2Position = (xP2 + 10,yP2) }
    else 
      game { p2Position = (-200, yP2) }
  where
    (xP2,yP2) = p2Position game
handleKeys (EventKey (Char 'i') _ _ _) game =
  if yP2 < 200 then
    game { p2Position = (xP2,yP2 + 10) }
    else game { p2Position = (xP2,200) }
  where
    (xP2,yP2) = p2Position game
handleKeys (EventKey (Char 'k') _ _ _) game =
  if yP2 > 0 then
    game { p2Position = (xP2,yP2 - 10) }
    else game { p2Position = (xP2, 0) }
  where
    (xP2,yP2) = p2Position game
handleKeys (EventKey (Char 'u') _ _ _) game = game { bulletsDownLoc = bulletsDownLoc' ++ [(xP2, yP2 - 10)] }
  where
    (xP2,yP2) = p2Position game
    bulletsDownLoc' = bulletsDownLoc game
handleKeys _ game = game

checkEnd :: MyGame -> MyGame
checkEnd game = 
    if not(length (bulletsDownLoc game) == length [ (i,j) | (i,j) <- bulletsDownLoc game, playerCollision (p1Position game) (i,j) 6] && length (bulletsTopLoc game) == length [ (i,j) | (i,j) <- bulletsTopLoc game, playerCollision (p1Position game) (i,j) 6]) then
      error "P2 WON"
      else if not(length (bulletsDownLoc game) == length [ (i,j) | (i,j) <- bulletsDownLoc game, playerCollision (p2Position game) (i,j) 6] && length (bulletsTopLoc game) == length [ (i,j) | (i,j) <- bulletsTopLoc game, playerCollision (p2Position game) (i,j) 6]) then
        error "P1 WON"
          else game

playerCollision :: (Float, Float) -> (Float, Float) -> Float -> Bool
playerCollision (px, py) (bx, by) radius = collision
  where
    collision = (sqrt ((px - bx)^2 + (py - by)^2)) - radius >= 5


wallCollision :: (Float, Float) -> Float -> Bool
wallCollision (x,y) radius = topCollision || bottomCollision
  where
    topCollision = y - radius <= -fromIntegral 206
    bottomCollision = y + radius >= fromIntegral 206

bulletsDismiss :: MyGame -> MyGame
bulletsDismiss game = game {
    bulletsDownLoc = [(i,j) | (i, j) <- bulletsDownLoc game, not $ wallCollision (i,j) 6],
    bulletsTopLoc = [(i,j) | (i, j) <- bulletsTopLoc game, not $ wallCollision (i,j) 6]
                           }

generateBullets :: MyGame -> MyGame
generateBullets game = if bulletsCount game == 80 then
  game {
    bulletsDownLoc = bulletsDownLoc game ++ [(fst (p1Position game), 200)],
    bulletsTopLoc  = bulletsTopLoc game ++ [(fst (p2Position game), -200)],
    bulletsCount = 0
  }
  else game { bulletsCount = bulletsCount game + 1 }

render :: MyGame -> Picture
render game = pictures [
    bulletsUp,
    bulletsDown,
    p1,
    p2
                       ]
    where
        bulletsDown = pictures [ uncurry translate (bullet) $ color ballColor $ circleSolid 6 | bullet <- bulletsDownLoc game ]
        bulletsUp = pictures [ uncurry translate (bullet) $ color ballColor $ circleSolid 6 | bullet <- bulletsTopLoc game ]
        p1 = player (p1Position game) p1Color
        p2 = player (p2Position game) p2Color

moveBullets :: Float -> MyGame -> MyGame
moveBullets sec state = state {
    bulletsDownLoc = [(i,j - (bulletsVel state * sec)) | (i, j) <- bulletsDownLoc state ],
    bulletsTopLoc = [(i,j + (bulletsVel state * sec)) | (i, j) <- bulletsTopLoc state ]
                              }


player :: (Float, Float) -> Color -> Picture
player (x,y) c = translate x y $ color c $ rectangleSolid 20 10

initialState :: MyGame
initialState = Game
  { bulletsDownLoc = []
  , bulletsTopLoc = []
  , bulletsVel  = 60
  , p1Position = (0, -100)
  , p2Position = (0, 100)
  , bulletsCount = 0
  }

data MyGame = Game
  { bulletsDownLoc :: [(Float, Float)]
  , bulletsTopLoc :: [(Float, Float)]
  , bulletsVel :: Float
  , p1Position :: (Float, Float)
  , p2Position :: (Float, Float)
  , bulletsCount :: Int
  } deriving Show

ballColor = dark red 
p1Color = light (light blue)
p2Color = light (light red)