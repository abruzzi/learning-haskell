let dict = [
        ('d', ("Dev", 0.5)), 
        ('D', ("Dev", 1.0)), 
        ('q', ("QA", 0.5)), 
        ('Q', ("QA", 1.0)), 
        ('b', ("Block", 0.5)), 
        ('B', ("Block", 1.0))
    ]

let expr = "ddDQq"

[y | (x, y) <- dict, z <- expr, x == z]

parse expr = [y | (x, y) <- dict, z <- expr, x == z]


---

let dict = [('d', ("Dev", 0.5)), ('D', ("Dev", 1.0)), ('q', ("QA", 0.5)), ('Q', ("QA", 1.0)), ('b', ("Block", 0.5)), ('B', ("Block", 1.0)) ]
parse expr = [y | z <- expr, (x, y) <- dict, x == z]

calc :: String -> [(String, Double)]
calc expr = [("QA", sum [y | (x, y) <- parse expr, x == "QA"]), ("Dev", sum [y | (x, y) <- parse expr, x == "Dev"]), ("Block", sum [y | (x, y) <- parse expr, x == "Block"]), ("Total", sum [y | (x, y) <- parse expr]) ]
    
---

parse :: [Char] -> [Map [Char] Double]
parse expr = [fromList (y) | z <- expr, (x, y) <- dict, x == z]

parse expr = Prelude.map fromList [y | z <- expr, (x, y) <- dict, x == z]

calc :: [x] -> [Map k a]
calc [] = fromList []
calc (h:t) = (fromList [h] ++ calc t) :


calc [] = []
calc (h:t) = h ++ calc t

calc [] = fromList []
calc (h:t) = 
    unionWith (+) 
    calc t

[y | (x, y) <- dict]
f (key, value) list = if key `elem` [(k, x) | (k, x) <- list] then [(k, x) | (k, x) <- list, k == key] else 0

reducer (key, value) co = 

foldr (\x c -> ) [] [("Dev", 0.5), ("Dev", 0.5)]


let dict = [('d', ("Dev", 0.5)), ('D', ("Dev", 1.0)), ('q', ("QA", 0.5)), ('Q', ("QA", 1.0)), ('b', ("Block", 0.5)), ('B', ("Block", 1.0)) ]

[y| (x, y) <- [("Dev", 0.5), ("QA", 0.5)], x `elem` [x | (_, (key, _)) <- dict]]

let list = [("Dev", 1.0), ("QA", 0.5)]

f key value list = 
    let val = [v|(k, v) <- list, k == key]
    list ++ (key, value + [val])

