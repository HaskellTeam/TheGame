import Matrix

-- FrameIteration is Fit
type Fit = Int

moveBlock :: Matrix -> Block -> Direction -> Matrix
moveBlock matrix blk West = matrix
moveBlock matrix blk East = matrix
moveBlock matrix blk South = matrix