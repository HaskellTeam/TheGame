{-
    Tetris The Best Game in The World!

    by: Brilhante, Costa, Nery
-}
module Main where

-- import Prelude hidding (Either(..))
import System.Console.ANSI
import System.IO
import Control.Concurrent
import Control.Monad
import Data.Maybe


main = do
    setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue ]
    putStrLn "Welcome to Tetris!"
    putStrLn "@"
    forkIO $ do
        handleInput
    gameloop


gameloop = do
    putStrLn "..."
    putStrLn "Input Usuario"
    putStrLn "Object Update"
    putStrLn "Cleaning"
    putStrLn "Rendering"
    threadDelay 500000
    gameloop

handleInput = do
    c <- newEmptyMVar 
    hSetBuffering stdin NoBuffering
    forkIO $ do
        a <- getChar
        putMVar c a
        putStrLn $ "\n" ++ [a]
    wait c
  where wait c = do
        a <- tryTakeMVar c
        if isJust a then handleInput
        else threadDelay 50000 >> wait c

