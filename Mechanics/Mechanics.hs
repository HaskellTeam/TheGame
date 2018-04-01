import Matrix
import Block

-- FrameIteration is Fit
type Fit = Int

moveBlock :: Matrix -> Block -> Direction -> Matrix
moveBlock matrix blk West = matrix
moveBlock matrix blk East = matrix
moveBlock matrix blk South = matrix


-- DATA STRUCTURES

data Hit = Hit {
    matrix :: Matrix,
    didHit :: Bool
}

-- MECHANINCS
updateMatrix :: Matrix -> Block -> Direction -> Hit
updateMatrix m b South = fallBlock b m
updateMatrix m b d = do
    let newMatrix = sideMove b m d
    Hit {
        matrix = m,
        didHit = False
    }

-- move o bloco para baixo se não atingir espaços ocupados
-- assenta o bloco se atingir um espaço ocupado
fallBlockHue :: Block -> Matrix -> Matrix
fallBlockHue (a, 0) m = paintBlockOnMatrix (a, 0) m
fallBlockHue b m = do
    -- testa se o bloco colide com a fileira y abaixo de si
    if hit (fst b, snd b - 1) m || hit (fst (r b), snd (r b) - 1) m
    then
        paintBlockOnMatrix b m
    else 
        fallBlock (move b South) m

-- move um bloco para baixo numa matriz, se possível
fallBlock :: Block -> Matrix -> Hit
fallBlock b m = do
    if hit (fst b, snd b - 1) m || hit (fst (r b), snd (r b) - 1) m
    then
        Hit {
        matrix = (paintBlockOnMatrix (fst b, snd b - 1) m),
        didHit = True
        }
    else
        Hit {
        matrix = (paintBlockOnMatrix (fst b, snd b - 1) m),
        didHit = False
        }



-- testa se a posição p de um bloco está sobre um espaço vazio ou ocupado da matriz
hit :: Position -> Matrix -> Bool
hit p m = True && getSpot p m

-- move um bloco lateralmente (se possível) na matriz
-- apenas movimentos laterias (West | East) surtem efeito nesta função
-- outros movimentos apenas retornarão a matriz imutada
sideMove :: Block -> Matrix -> Direction -> Matrix
sideMove b m d = m
sideMove b m West = do
    if hit (move b West) m || hit (move (u b) West) m
    then paintBlockOnMatrix b m
    else paintBlockOnMatrix (move b West) m

sideMove b m East = do
    if hit (move (r b) East) m || hit (move (ur b) East)
    then paintBlockOnMatrix b m
    else paintBlockOnMatrix (move b West) m
