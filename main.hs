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

inputTimeout = 10000


main = do
    setCursorPosition 5 0
    setTitle "Tetris THE GAME"
    setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue ]
    putStrLn "Welcome to Tetris!"
    putStrLn "@"
    gameloop matrix enterBlock 0


gameloop :: Matrix -> Block -> Int -> IO ()
gameloop m b it = do

    let upd_it = mod it 41
    let upd_matrix = checkAndDestroy m (length m)

    nothing <- newEmptyMVar
    hit <- newEmptyMVar

    if ( ((mod upd_it 20) == 0) && (upd_it /= 0) ) -- Equals 10 and not equals 0
        then putMVar hit (updateMatrix upd_matrix b South) -- Apply gravity every 10 frames
        else putMVar hit (updateMatrix upd_matrix b None)

    hFlush stdout
    hSetBuffering stdin NoBuffering

    -- Handle Input
    c <- timeout inputTimeout getChar

    case c of
        Nothing -> do putMVar nothing (updateMatrix upd_matrix b None)
        Just 'q' -> do
            putStrLn "Update game loop Quit State..."
            exitSuccess
        Just 'r' -> do
            gameloop matrix enterBlock 0 -- Restart The Game
        Just input -> do
            var_hit <- (takeMVar hit)
            putMVar hit (updateMatrix (matrixOf var_hit) b (inputMove input))


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
        when (didLose (matrixOf actualHit)) exitSuccess 
        gameloop (matrixOf actualHit) enterBlock (upd_it + 1) -- Updates game loop when block touches another block or hit ground
    else do
        gameloop upd_matrix (blockOf actualHit) (upd_it + 1) -- Updates game loop when nothing happens

inputMove :: Char -> Direction
inputMove 'a' = West
inputMove 's' = South
inputMove 'd' = East
inputMove _ = None

