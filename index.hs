module Index where

import Control.Concurrent
import Control.Concurrent.MVar

import Text.Printf

-- horizontal :: () -> [String]
-- horizontal _ = ["|", " ", " ", " ", " ", " ", " ", " ", " ", " ", ]

-- intializeMap :: Int -> Int -> [[String]] -> [[String]]
-- intializeMap 0 0 map = map
-- intializeMap i 10 map = intializeMap i + 1 0 map


main = do
    show (topBoundary 15 [])
    show (bottomBoundary 15 15 [])
    -- map <- newEmptyMVar

topBoundary :: Int -> [Char] -> [Char]
topBoundary 0 mapa = mapa ++ "\n"
topBoundary num mapa = topBoundary (num-1) mapa ++ "="

bottomBoundary :: Int -> Int -> [Char] -> [Char]
bottomBoundary num max mapa
    | num == 0   = mapa ++ "|" ++ "\n"
    | num == max = bottomBoundary (num-1) max (mapa ++ "|")
    | otherwise  = bottomBoundary (num-1) max (mapa ++ "=")

-- middleSpace :: Int -> Int -> [Char] -> [Char]
-- middleSpace num max mapa =
--     | num == 0   = mapa ++ "|"
--     | num == max = middleSpace (num-1) max (mapa ++ "|")
--     | otherwise  = middleSpace (num-1) max (mapa ++ "_")

-- bottomBoundary :: Int -> Int -> [Char] -> [Char]
-- bottomBoundary num max mapa =
--     | num == 0   = mapa ++ "|"
--     | num == max = bottomBoundary (num-1) max (mapa ++ "|")
--     | otherwise  = bottomBoundary (num-1) max (mapa ++ "_")
    
dummyFunction :: Int -> [Char] -> [Char]
dummyFunction 0 str = str
dummyFunction n str = dummyFunction (n-1) (str ++ ".")
