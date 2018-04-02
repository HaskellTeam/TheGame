{-
    Tetris The Best Game in The World!

    by: Brilhante, Costa, Nery
-}
module Main where

-- import Prelude hidding (Either(..))
import System.Console.ANSI
import System.IO
import System.Timeout
import Control.Concurrent
import Control.Monad
import Data.Maybe
import Mechanics.Mechanics
import Mechanics.Matrix
import Mechanics.Block

inputTimeout = 300000


main = do
    setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue ]
    putStrLn "Welcome to Tetris!"
    putStrLn "@"
    gameloop matrix enterBlock


gameloop :: Matrix -> Block
gameloop m b = do
    putStrLn "..."
    hFlush stdout
    hSetBuffering stdin NoBuffering
    
    -- Handle Input
    c <- timeout inputTimeout getChar
    hit <- newEmptyVar
    case c of
        Nothing -> do putMVar hit updateMatrix m b None
        Just 'q' -> do
            putStrLn "Update game loop Quit State..."
            return
        Just input -> do 
            putMVar hit updateMatrix m b (inputMove input)
    
    putStrLn "Object Update"
    putStrLn "Cleaning"
    putStrLn "Rendering"
    threadDelay inputTimeout

    if didLayDown hit
    then gameloop matrixOf hit enterBlock
    else gameloop m bockOf hit

inputMove :: Char -> Direction
inputMove 'a' = West
inputMove 's' = South
inputMove 'd' = East
inputMove _ = None

