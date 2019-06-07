module Index where

import Control.Concurrent
import Control.Concurrent.MVar

main = do
    -- printLine $ topBoundary 15 []
    -- printLine $ regularSpace 15 15 []
    -- printLine $ regularSpace 15 15 []
    -- printLine $ regularSpace 15 15 []
    -- printLine $ middleSpace 15 15 []
    -- printLine $ regularSpace 15 15 []
    -- printLine $ regularSpace 15 15 []
    -- printLine $ regularSpace 15 15 []
    -- printLine $ bottomBoundary 15 15 []
    mapa <- newEmptyMVar
    putMVar mapa $ generateMap 16 0 []
    linha <- takeMVar mapa
    mapM_ printLine $ linha

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