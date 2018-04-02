module Block where

    -- Block representa a posição esquerda inferior de uma matriz 2x2
    -- u representa a posição esquerda superior de uma matriz 2x2, em relação a um block
    -- r representa a posição direita inferior de uma matriz 2x2, em relação a um block
    -- ur representa a posição direita superior de uma matriz 2x2, em relação a um block

    -- Direction representa a direção de um movimento de um Block
data Direction = None
    | West
    | East
    | South

-- Position representa um ponto num plano Cartesiano (x, y)
type Position = (Int, Int)

type Block = Position

u :: Block -> Position
u block = ( fst block, snd block + 1)

r :: Block -> Position
r block = ( fst block + 1, snd block)

ur :: Block -> Position
ur block = ( fst block + 1, snd block + 1)

project :: Block -> Direction -> Block
project b West = (fst b - 1, snd b)
project b East = (fst b + 1, snd b)
project b South = (fst b, snd b - 1)
project b None = b
