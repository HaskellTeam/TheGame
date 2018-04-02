module Mechanics.Mechanics where

import Mechanics.Matrix
import Mechanics.Block

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
updateMatrix m b d = move b d m

-- testa se a posição p de um bloco está sobre um espaço vazio ou ocupado da matriz
hit :: Position -> Matrix -> Bool
hit p m = True && getSpot p m

-- move um bloco (se possível) na matriz.
move :: Block -> Direction -> Matrix ->  Hit
move b None m = Hit {
    matrixOf = m,
    blockOf = b,
    didLayDown = False
}
move b West m = do
    if hit (project b West) m || hit (project (u b) West) m
    then Hit {
        matrixOf = paintBlockOnMatrix b m,
        blockOf = b,
        didLayDown = False
    } 
    else Hit {
        matrixOf = paintBlockOnMatrix (project b West) m,
        blockOf = (project b West),
        didLayDown = False
    }

move b East m = do
    if hit (project (r b) East) m || hit (project (ur b) East) m
    then Hit {
        matrixOf = paintBlockOnMatrix b m,
        blockOf = b,
        didLayDown = False
    } 
    else Hit {
        matrixOf = paintBlockOnMatrix (project b East) m,
        blockOf = (project b East),
        didLayDown = False
    }

move b South m = do
    if hit (project b South) m || hit (project (r b) South) m
    then
        Hit {
            matrixOf = (paintBlockOnMatrix b m),
            blockOf = b,
            didLayDown = True
        }
    else
        Hit {
            matrixOf = (paintBlockOnMatrix (project b South) m),
            blockOf = (project b South),
            didLayDown = False
        }
    


    
