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
hitBlock :: Position -> Matrix -> Bool
hitBlock p m = True && getSpot p m

-- move um bloco (se possível) na matriz.
move :: Block -> Direction -> Matrix ->  Hit

move (4,n) East m = Hit {
    matrixOf = m,
    blockOf = (4,n),
    didLayDown = False
}
move (0,n) West m = Hit {
    matrixOf = m,
    blockOf = (0,n),
    didLayDown = False
}
move b None m = Hit {
    matrixOf = m,
    blockOf = b,
    didLayDown = False
}
move b West m = do
    if hitBlock (project b West) m || hitBlock (project (u b) West) m
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
    if hitBlock (project (r b) East) m || hitBlock (project (ur b) East) m
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
    if hitBlock (project b South) m || hitBlock (project (r b) South) m
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

destroyLine :: Int -> Matrix -> Matrix
destroyLine i m = (take (i) m) ++ (drop (i + 1) m) ++ empty


checkLine :: Row -> Bool
checkLine row = head row && checkline (tail row)

checkAndDestroy :: Matrix -> Int -> Matrix
checkAndDestroy m 0 = m
checkAndDesroy m len = do
    if checkLine m!!(len -1)
    then destroyLine
    else checkAndDestroy m (len -1)
    


    
