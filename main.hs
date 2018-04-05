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


gameloop :: Matrix -> Block -> IO ()
gameloop m b = do
    hFlush stdout
    hSetBuffering stdin NoBuffering
    
    -- Handle Input
    c <- timeout inputTimeout getChar
    hit <- newEmptyMVar
    case c of
        Nothing -> do putMVar hit (updateMatrix m b None)
        Just 'q' -> do
            putStrLn "Update game loop Quit State..."
        Just input -> do
            putMVar hit (updateMatrix m b (inputMove input))
    
    -- Clear Screen 
    clearScreen

    -- Render screen
    -- printMatrix m (length m)

    -- game delay
    threadDelay inputTimeout

    do 
    actualHit <- (takeMVar hit)
    if didLayDown actualHit
    then do
        printMatrix (matrixOf actualHit ) (length m)
        gameloop (matrixOf actualHit ) enterBlock
    else do
        printMatrix m (length m)
        gameloop m (blockOf actualHit )

inputMove :: Char -> Direction
inputMove 'a' = West
inputMove 's' = South
inputMove 'd' = East
inputMove _ = None

