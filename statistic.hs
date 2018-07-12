module Main where
    
    data Status = Dev | QA | Block deriving (Show)
    type Mark = (Status, Double)

    value :: Char -> Mark

    value 'd' = (Dev, 0.5)
    value 'D' = (Dev, 1.0)
    value 'q' = (QA, 0.5)
    value 'Q' = (QA, 1.0)
    value 'b' = (Block, 0.5)
    value 'B' = (Block, 1.0)