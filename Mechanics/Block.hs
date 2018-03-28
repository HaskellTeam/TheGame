module Block where

type Position = (Int, Int)

type Block = Position

u :: Block -> Position
u block = ( fst block, snd block + 1)

r :: Block -> Position
r block = ( fst block + 1, snd block)

ur :: Block -> Position
ur block = ( fst block + 1, snd block + 1)

moveDown :: Block -> Block
moveDown b = (fst b, snd b + 1)