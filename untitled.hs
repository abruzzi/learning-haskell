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