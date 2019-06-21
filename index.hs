module Index where

import Control.Concurrent
import Control.Concurrent.MVar
import Data.List

main = do
    mapa <- newEmptyMVar
    bullets <- newEmptyMVar
    players <- newEmptyMVar
    putMVar mapa $ generateMap 16 0 []
    field <- takeMVar mapa
    let p1 = newPlayer 3 2 '1'
    let p2 = newPlayer 4 0 '2'
    let b1 = newBullet 5 5
    putMVar players [p1, p2]
    putMVar bullets [b1]
    mapM_ printLine $ field
    putMVar mapa field
    field <- takeMVar mapa
    putMVar mapa (setPositionMap field b1 '*') 
    field <- takeMVar mapa
    mapM_ printLine $ setPositionPlayer field p1
    print $ show $ isPlayerDead [b1] p1
    return ()

isPlayerDead :: [(Int, Int)] -> (Int, Int, Char) -> Bool
isPlayerDead [] (playerLine, playerCol, name)          = False
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