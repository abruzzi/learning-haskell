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

let dict = [('d', ("Dev", 0.5)), ('D', ("Dev", 1.0)), ('q', ("QA", 0.5)), ('Q', ("QA", 1.0)), ('b', ("Block", 0.5)), ('B', ("Block", 1.0)) ]

parse :: String -> [(String, Double)]
parse expr = [y | z <- expr, (x, y) <- dict, x == z]

calc :: String -> [(String, Double)]

calc expr = 
    [
    ("QA", sum [y | (x, y) <- parse expr, x == "QA"]), 
    ("Dev", sum [y | (x, y) <- parse expr, x == "Dev"]), 
    ("Block", sum [y | (x, y) <- parse expr, x == "Block"]), 
    ("Total", sum [y | (x, y) <- parse expr]) 
]