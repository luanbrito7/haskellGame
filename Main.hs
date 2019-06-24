import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

main :: IO ()
main = simulate window background fps initialState render update
    where
        update :: ViewPort -> Float -> MyGame -> MyGame
        update _ secs = bulletsDismiss . moveBullets secs
        window = InWindow "Game" (400, 400) (0, 0) 
        background = black

fps :: Int
fps = 60

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
    bulletsDown
    player (0, 200) p1Color,
    player (0, -200) p2Color
                       ]
    where
        bulletsDown = [ uncurry translate (bullet) $ color ballColor $ circleSolid 15 | bullet <- bulletsDownLoc game ]
        bulletsUp = [ uncurry translate (bullet) $ color ballColor $ circleSolid 15 | bullet <- bulletsTopLoc game ]

moveBullets :: Float -> MyGame -> MyGame
moveBullets sec state = state {
    bulletsDownLoc = [(i,j - (bulletsVel state * sec)) | (i, j) <- bulletsDownLoc state ],
    bulletsTopLoc = [(i,j + (bulletsVel state * sec)) | (i, j) <- bulletsTopLoc state ]
                              }

bullet :: (Float, Float) -> Picture
bullet (x,y) = translate x y $ color ballColor $ circleSolid 15

player :: (Float, Float) -> Color -> Picture
player (x,y) c = translate x y $ color c $ rectangleSolid 20 10

initialState :: MyGame
initialState = Game
  { bulletsDownLoc = [(-40, -200), (30, 200)]
  , bulletsTopLoc = [(-40, -200), (30, 200)]
  , bulletsVel  = 2
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