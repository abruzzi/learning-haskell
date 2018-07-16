import Data.Map as Map

module Main where
	let dict = [('d', ("Dev", 0.5)), ('D', ("Dev", 1.0)), ('q', ("QA", 0.5)), ('Q', ("QA", 1.0)), ('b', ("Block", 0.5)), ('B', ("Block", 1.0)) ]
	parse :: String -> Map String Double
	parse expr = [fromList (y) | z <- expr, (x, y) <- dict, x == z]
