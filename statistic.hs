dict = [
  ('d', ("Dev", 0.5)), 
  ('D', ("Dev", 1.0)), 
  ('q', ("QA", 0.5)), 
  ('Q', ("QA", 1.0)), 
  ('b', ("Block", 0.5)), 
  ('B', ("Block", 1.0))
  ]

parse :: String -> [(String, Double)]
parse expr = [y | z <- expr, (x, y) <- dict, x == z]


inStatus result status = (status, sum [y | (x, y) <- result, x == status])

calc :: String -> [(String, Double)]
calc expr = [
  inStatus (parse expr) "QA", 
  inStatus (parse expr) "Dev",
  inStatus (parse expr) "Block",
  ("Total", sum [y | (x, y) <- parse expr]) 
  ]

-- macro result field = (@field, sum [y | (x, y) <- result, x == @field)
-- calc expr = [macro result "QA"]

-- zip [1..] ["apple", "orange", "cherry", "mango"]  