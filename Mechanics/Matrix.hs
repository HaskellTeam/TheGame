
module Mechanics.Matrix where
    -- A matriz do jogo é uma matriz 5x5 de valores booleanos
    -- False representa um espaço vazio e True representa um espaço ocupado
    -- Blocks entram na posição (1, 3), sendo x um valor arbitrário entre 0 e 3
    -- Bloks caem até a posição (x, y), sendo x um valor arbitrário entre 0 e 3 e y o valor True mais alto sob o bloco

import Mechanics.Block

-- DATA STRUCTURES

type Row = [Bool] -- False = espaço vazio | True = espaço ocupado

type Matrix = [Row]

-- data Game = EndGame | Game Matrix
--     deriving (Show)


-- fileira vazia
empty :: Row
empty = [False, False, False, False, False]

-- matriz padrão
matrix :: Matrix
matrix = [empty, empty, empty, empty, empty]


-- MATRIX HELPERS
-- funções para auxiliar a acessar e modificar a matriz

-- swap sublist on `list` with `content`
-- sublist to be swaped inside `list` is defined by `index` and `size`
-- example: swap [0, 1, 2, 3, 4, 5, 6] 3 2 [9, 9] results in [0, 1, 2, 9, 9, 5, 6]
swap list index size content = (take index list) ++ content ++ (drop (index + size) list)


-- assenta um bloco na matriz, ou seja, muda o espaço ocupado por um bloco de vazio (False) para ocupado (True) dentro de uma matriz
paintBlockOnMatrix :: Block -> Matrix -> Matrix
paintBlockOnMatrix b m = do
    let row = (swap (m!!(snd b)) (fst b) 2 (replicate 2 True))
    let uRow = (swap (m!!(snd (u b))) (fst b) 2 (replicate 2 True))
    swap m (snd b) 2 [row, uRow]

-- retorna o conteúdo na posição p (x, y) em uma matriz
getSpot :: Position -> Matrix -> Bool
getSpot p m = (m!!(snd p))!!(fst p)

-- HUE
-- coisas que possivelmente irão deixar de existir/ser modificadas/ser movidas para outro lugar

-- gameRecursion :: Game -> IO()
-- gameRecursion EndGame = print "hue"
-- gameRecursion (Game m) = do
--     printMatrix m (length m)

printMatrix :: Matrix -> Int -> IO ()
printMatrix matrix 0 = print("---------------------------end")
printMatrix matrix len = do
    print(matrix!!(len - 1))
    printMatrix matrix (len - 1)