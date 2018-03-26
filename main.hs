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

inputTimeout = 300000


main = do
    setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue ]
    putStrLn "Welcome to Tetris!"
    putStrLn "@"
    gameloop


gameloop = do
    putStrLn "..."
    hFlush stdout
    hSetBuffering stdin NoBuffering
    
    -- Handle Input
    c <- timeout inputTimeout getChar
    case c of
        Nothing -> do putStrLn "Update game loop"
        Just 'q' -> do putStrLn "Update game loop Quit State..."
        Just input -> do putStrLn $ "Update game loop with input: " ++ [input]
    
    putStrLn "Object Update"
    putStrLn "Cleaning"
    putStrLn "Rendering"
    threadDelay inputTimeout

    gameloop
