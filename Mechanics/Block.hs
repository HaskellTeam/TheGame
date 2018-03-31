module Block where

data Direction = None
    | West
    | East
    | South

type Position = (Int, Int)

type Block = Position

u :: Block -> Position
u block = ( fst block, snd block + 1)

r :: Block -> Position
r block = ( fst block + 1, snd block)

ur :: Block -> Position
ur block = ( fst block + 1, snd block + 1)

move :: Block -> Direction -> Block
move b West = (fst b - 1, snd b)
move b East = (fst b + 1, snd b)
move b South = (fst b, snd b + 1)
