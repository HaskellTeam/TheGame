import Block

type Row = [Bool] -- False = espaço vazio | True = espaço ocupado

type Matrix = [Row]

data Game = EndGame | Game Matrix
    deriving (Show)


empty :: Row
empty = [False, False, False, False, False]
hue = [False, False, True, False, False]

matrix:: Matrix
matrix = [empty, empty, empty, hue, empty]

-- paintRow :: Row -> Int -> Int -> Bool -> Row
-- paintRow row qty index value = (take index row) ++ (replicate qty value) ++ (drop (index + qty) row)

-- swap sublist on `list` starting at `index` with `size` size with `content`
-- example: swap [0, 1, 2, 3, 4, 5, 6] 3 2 [9, 9] results in [0, 1, 2, 9, 9, 5, 6]
swap list index size content = (take index list) ++ content ++ (drop (index + size) list)

paintBlockOnMatrix :: Block -> Matrix -> Matrix
paintBlockOnMatrix b m = swap m (snd b) 2 [(swap (m!!(snd b)) (fst b) 2 (replicate 2 True)), (swap (m!!(snd (u b))) (fst b) 2 (replicate 2 True))]

-- retorna o conteúdo na posição p em uma matriz
getSpot :: Position -> Matrix -> Bool
getSpot p m = (m!!(snd p))!!(fst p)

printMatrix :: Matrix -> Int -> IO()
printMatrix matrix 0 = print("---------------------------end")
printMatrix matrix len = do
    print(matrix!!(len - 1))
    printMatrix matrix (len - 1)

-- testa se a posição p de um bloco está sobre um espaço vazio ou ocupado da matriz
hit :: Position -> Matrix -> Bool
hit p m = True && getSpot p m

fallBlock :: Block -> Matrix -> Matrix
fallBlock b m = do
    -- printMatrix m (length m)
    if hit b m || hit (r b) m
    then paintBlockOnMatrix b m
    else fallBlock (moveDown b) m

gameRecursion :: Game -> IO()
gameRecursion EndGame = print "hue"
gameRecursion (Game m) = do
    printMatrix m (length m)