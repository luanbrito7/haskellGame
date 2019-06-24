import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play window background fps initialState render handleKeys update
    where
        update :: Float -> MyGame -> MyGame
        update secs = bulletsDismiss . moveBullets secs
        window = InWindow "Game" (400, 400) (0, 0) 
        background = black

fps :: Int
fps = 60

handleKeys :: Event -> MyGame -> MyGame
handleKeys (EventKey (Char 'a') _ _ _) game = game { p1Position = (xP1 - 10,yP1) }
  where
    (xP1,yP1) = p1Position game
handleKeys (EventKey (Char 'd') _ _ _) game = game { p1Position = (xP1 + 10,yP1) }
  where
    (xP1,yP1) = p1Position game
handleKeys (EventKey (Char 'j') _ _ _) game = game { p2Position = (xP2 - 10,yP2) }
  where
    (xP2,yP2) = p2Position game
handleKeys (EventKey (Char 'l') _ _ _) game = game { p2Position = (xP2 + 10,yP2) }
  where
    (xP2,yP2) = p2Position game
handleKeys _ game = game

wallCollision :: (Float, Float) -> Float -> Bool
wallCollision (x,y) radius = topCollision || bottomCollision
  where
    topCollision = y - radius <= -fromIntegral 200
    bottomCollision = y + radius >= fromIntegral 200

bulletsDismiss :: MyGame -> MyGame
bulletsDismiss game = game {
    bulletsDownLoc = [(i,j) | (i, j) <- bulletsDownLoc game, not $ wallCollision (i,j) 15],
    bulletsTopLoc = [(i,j) | (i, j) <- bulletsTopLoc game, not $ wallCollision (i,j) 15]
                           }

render :: MyGame -> Picture
render game = pictures [
    bulletsUp,
    bulletsDown,
    p1,
    p2
                       ]
    where
        bulletsDown = pictures [ uncurry translate (bullet) $ color ballColor $ circleSolid 15 | bullet <- bulletsDownLoc game ]
        bulletsUp = pictures [ uncurry translate (bullet) $ color ballColor $ circleSolid 15 | bullet <- bulletsTopLoc game ]
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
  { bulletsDownLoc = [(-40, 180), (30, 180)]
  , bulletsTopLoc = [(-40, -180), (30, -180)]
  , bulletsVel  = 10
  , p1Position = (0, -200)
  , p2Position = (0, 200)
  }

data MyGame = Game
  { bulletsDownLoc :: [(Float, Float)]
  , bulletsTopLoc :: [(Float, Float)]
  , bulletsVel :: Float
  , p1Position :: (Float, Float)
  , p2Position :: (Float, Float)
  } deriving Show

ballColor = dark red 
p1Color = light (light blue)
p2Color = light (light red)