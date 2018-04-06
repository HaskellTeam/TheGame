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
import System.Exit

inputTimeout = 300000


main = do
    setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue ]
    putStrLn "Welcome to Tetris!"
    putStrLn "@"
    gameloop matrix enterBlock


gameloop :: Matrix -> Block -> IO ()
gameloop m b = do

    let upd_matrix = checkAndDestroy m (length m)

    hFlush stdout
    hSetBuffering stdin NoBuffering
    
    -- Handle Input
    c <- timeout inputTimeout getChar
    hit <- newEmptyMVar
    
    case c of
        Nothing -> do putMVar hit (updateMatrix upd_matrix b None)
        Just 'q' -> do
            putStrLn "Update game loop Quit State..."
            exitSuccess
        Just input -> do
            putMVar hit (updateMatrix upd_matrix b (inputMove input))

    -- Just Clears the Screen... nothing more
    clearScreen

    -- Render screen
    printMatrix ( paintBlockOnMatrix b upd_matrix ) (length upd_matrix )

    -- game delay
    threadDelay inputTimeout
    
    do 
    actualHit <- (takeMVar hit)
    if didLayDown actualHit
    then do
        gameloop (matrixOf actualHit) enterBlock -- Updates game loop when block touches another block or hit ground
    else do
        gameloop upd_matrix (blockOf actualHit) -- Updates game loop when nothing happens

inputMove :: Char -> Direction
inputMove 'a' = West
inputMove 's' = South
inputMove 'd' = East
inputMove _ = None

