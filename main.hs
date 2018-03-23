{-
    Tetris The Best Game in The World!

    by: Brilhante, Costa, Nery
-}
module Main where

-- import Prelude hidding (Either(..))
import System.Console.ANSI
import System.IO
import Control.Concurrent


main = do
    setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue ]
    putStrLn "Welcome to Tetris!"
    putStrLn "@"
    gameloop


gameloop = do
    putStrLn "..."
    putStrLn "Input Usuario"
    putStrLn "Object Update"
    putStrLn "Cleaning"
    putStrLn "Rendering"
    threadDelay 500000
    gameloop

