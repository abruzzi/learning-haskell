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

---
--- curruring could be very interesting
---

-- base expr status = (status, sum [y | (x, y) <- [detail x | x <- expr], x == status])
-- inStatus = base "dddDDd"

-- inStatus "QA"
-- inStatus "Dev"


--- calc (parse "dddDDd")
--- 

sumByStatus expr status = (status, sum [y | (x, y) <- [(x, 1) | x <- expr, x == status]])
output (code, amount) = 
  let 
    (status, weight) = detail code
  in
    (status, amount * weight)


-- withExpr = sumByStatus "dddDDd"
-- withExpr 'D'

-- (status, weight) <- detail x