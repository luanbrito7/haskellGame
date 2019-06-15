module Index where

import Control.Concurrent
import Control.Concurrent.MVar

main = do
    mapa <- newEmptyMVar
    bullets <- newEmptyMVar
    players <- newEmptyMVar
    putMVar mapa $ generateMap 16 0 []
    linha <- takeMVar mapa
    let p1 = newPlayer 3 2 "p1"
    let p2 = newPlayer 4 0 "p2"
    let b1 = newBullet 1 2
    putMVar players [p1, p2]
    putMVar bullets [b1]
    mapM_ printLine $ linha

newBullet :: Int -> Int -> (Int, Int)
newBullet x y = (x,y)

newPlayer :: Int -> Int -> String -> (Int, Int, String)
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