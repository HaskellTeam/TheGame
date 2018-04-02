module Mechanics where

import Matrix
import Block

-- FrameIteration is Fit
type Fit = Int

-- DATA STRUCTURES

data Hit = Hit {
    matrixOf :: Matrix,
    blockOf :: Block,
    didLayDown :: Bool
}

-- MECHANINCS
updateMatrix :: Matrix -> Block -> Direction -> Hit
updateMatrix m b d = sideMove b d m

-- testa se a posição p de um bloco está sobre um espaço vazio ou ocupado da matriz
hit :: Position -> Matrix -> Bool
hit p m = True && getSpot p m

-- move um bloco (se possível) na matriz.
move :: Block -> Direction -> Matrix ->  Hit
move b None m = m
move b West m = do
    if hit (project b West) m || hit (project (u b) West) m
    then Hit {
        matrixOf = paintBlockOnMatrix b m,
        blockOf = b,
        didLayDown = false
    } 
    else Hit {
        matrixOf = paintBlockOnMatrix (project b West) m,
        blockOf = (project b West),
        didLayDown = false
    }

move b m East = do
    if hit (project (r b) East) m || hit (project (ur b) East)
    then Hit {
        matrixOf = paintBlockOnMatrix b m,
        blockOf = b,
        didLayDown = false
    } 
    else Hit {
        matrixOf = paintBlockOnMatrix (project b East) m,
        blockOf = (project b East),
        didLayDown = false
    }

move b m South = do
    if hit (move b South) m || hit (move (r b) South) m
    then
        Hit {
            matrix = (paintBlockOnMatrix b m),
            block = b,
            didHit = True
        }
    else
        Hit {
            matrix = (paintBlockOnMatrix (project b South) m),
            block = (move b south),
            didHit = False
        }
    


    
