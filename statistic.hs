detail :: Char -> (String, Double)
detail 'd' = ("Dev", 0.5)
detail 'D' = ("Dev", 1.0)
detail 'q' = ("QA", 0.5) 
detail 'Q' = ("QA", 1.0) 
detail 'b' = ("Block", 0.5)
detail 'B' = ("Block", 1.0)
detail x = ([], 0.0)

parse :: String -> [(String, Double)]
parse expr = [detail x | x <- expr]

inStatus result status = (status, sum [y | (x, y) <- result, x == status])

calc :: [(String, Double)] -> [(String, Double)]
calc timesheet = [
  inStatus timesheet "Dev",
  inStatus timesheet "QA", 
  inStatus timesheet "Block",
  ("Total", sum [y | (x, y) <- timesheet]) 
  ]

-- macro result field = (@field, sum [y | (x, y) <- result, x == @field)
-- calc expr = [macro result "QA"]

-- zip [1..] ["apple", "orange", "cherry", "mango"]  