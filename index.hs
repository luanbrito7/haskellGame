module Index where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.List

main = do
    mapa <- newEmptyMVar
    bulletsTop <- newEmptyMVar
    bulletsBottom <- newEmptyMVar
    players <- newEmptyMVar
    putMVar mapa $ generateMap 16 0 []
    let p1 = newPlayer 3 2 '1'
    let p2 = newPlayer 4 0 '2'
    let b1 = newBullet 1 2
    let b2 = newBullet 1 5
    let b3 = newBullet 15 4
    putMVar players [p1, p2]
    putMVar bulletsTop [b1, b2]
    putMVar bulletsBottom [b3]
    forkIO (updateState mapa bulletsTop bulletsBottom players)
    return ()

updateState :: MVar [[Char]] -> MVar [(Int, Int)] -> MVar [(Int, Int)] -> MVar [(Int, Int, Char)] -> IO ()
updateState mapa bulletsDown bulletsUp players = do
    field <- takeMVar mapa
    bd <- takeMVar bulletsDown
    bu <- takeMVar bulletsUp
    putMVar mapa $ bulletsAdvance field bd "down"
    field <- takeMVar mapa
    putMVar mapa $ bulletsAdvance field bu "up"
    field <- readMVar mapa
    putMVar bulletsUp $ updateBullets field bu "up" []
    putMVar bulletsDown $ updateBullets field bd "down" []
    mapM_ printLine $ field
    threadDelay 900000
    updateState mapa bulletsDown bulletsUp players

updateBullets :: [[Char]] -> [(Int, Int)] -> String -> [(Int, Int)] -> [(Int, Int)]
updateBullets mapa [] direction updatedBullets     = updatedBullets
updateBullets mapa ((x,y):xs) direction updatedBullets = do
    let posDown = getPosition mapa (x+1, y)
    let posUp = getPosition mapa (x-1, y)
    if direction == "down" then do
        if isMapLimits posDown then
            updateBullets mapa xs direction updatedBullets
            else if '-' == posDown then
                updateBullets mapa xs direction (updatedBullets ++ [(x+2, y)])

            else do
                updateBullets mapa xs direction (updatedBullets ++ [(x+1, y)])
    else do
        if isMapLimits posUp then
            updateBullets mapa xs direction updatedBullets
            else if '-' == posUp then
                updateBullets mapa xs direction (updatedBullets ++ [(x-2, y)])
            else do
                updateBullets mapa xs direction (updatedBullets ++ [(x-1, y)])

bulletsAdvance :: [[Char]] -> [(Int, Int)] -> String -> [[Char]]
bulletsAdvance field [] direction = field
bulletsAdvance field ((x,y):xs) direction = do
    let posDown = getPosition field (x+1, y)
    let posUp = getPosition field (x-1, y)
    if direction == "down" then do
        let mp1 = setPositionMap field (x,y) ' '
        if isMapLimits posDown || (posDown == '-') then
            bulletsAdvance mp1 xs direction
            else bulletsAdvance (setPositionMap mp1 (x+1,y) '*') xs direction
        else do
            let mp1 = setPositionMap field (x,y) ' '
            if isMapLimits posUp || (posUp == '-') then
                bulletsAdvance mp1 xs direction
            else bulletsAdvance (setPositionMap mp1 (x-1, y) '*') xs direction

isMapLimits :: Char -> Bool
isMapLimits character = do
    if character == '|' || character == '_' then
        True
        else False

isPlayerDead :: [(Int, Int)] -> (Int, Int, Char) -> Bool
isPlayerDead [] (playerLine, playerCol, name)           = False
isPlayerDead ((x,y):tail) (playerLine, playerCol, name) = do
    if x == playerLine && y == playerCol then
        True
        else isPlayerDead tail (playerLine, playerCol, name)

getPosition :: [[Char]] -> (Int, Int) -> Char
getPosition field (line, col) = (field !! line) !! (col)

setPositionMap :: [[Char]] -> (Int, Int) -> Char -> [[Char]]
setPositionMap field (line, col) newValue = take line field ++ [setPositionList (field !! line) col newValue] ++ drop (line+1) field

setPositionPlayer :: [[Char]] -> (Int, Int, Char) -> [[Char]]
setPositionPlayer field (line, col, name) = take line field ++ [setPositionList (field !! line) col name] ++ drop (line+1) field

setPositionList :: [Char] -> Int -> Char -> [Char]
setPositionList line index newElement = take index line ++ [newElement] ++ drop (index + 1) line

newBullet :: Int -> Int -> (Int, Int)
newBullet x y = (x,y)

newPlayer :: Int -> Int -> Char -> (Int, Int, Char)
newPlayer x y name = (x,y,name)

generateMap :: Int -> Int -> [[Char]] -> [[Char]]
generateMap max num mapa
    | num == 0           = [topBoundary max []] ++ (generateMap max (num+1) mapa) 
    | num == (div max 2) = [middleSpace max max []] ++ (generateMap max (num+1) mapa) 
    | num == max         = mapa ++ [bottomBoundary max max []]
    | otherwise          = [regularSpace max max []] ++ (generateMap max (num+1) mapa)

topBoundary :: Int -> [Char] -> [Char]
topBoundary 0 mapa = mapa ++ "\n"
topBoundary num mapa = topBoundary (num-1) mapa ++ "_"

bottomBoundary :: Int -> Int -> [Char] -> [Char]
bottomBoundary num max mapa
    | num == 1   = mapa ++ "|" ++ "\n"
    | num == max = bottomBoundary (num-1) max (mapa ++ "|")
    | otherwise  = bottomBoundary (num-1) max (mapa ++ "_")

middleSpace :: Int -> Int -> [Char] -> [Char]
middleSpace num max mapa
    | num == 1   = mapa ++ "|"
    | num == max = middleSpace (num-1) max (mapa ++ "|")
    | otherwise  = middleSpace (num-1) max (mapa ++ "-")

regularSpace :: Int -> Int -> [Char] -> [Char]
regularSpace num max mapa
    | num == 1   = mapa ++ "|"
    | num == max = regularSpace (num-1) max (mapa ++ "|")
    | otherwise  = regularSpace (num-1) max (mapa ++ " ")

printLine :: [Char] -> IO () -- Essa função vai receber cada linha do MVAR e ir printando.
printLine str = putStrLn (str)