primes = filterPrime [2..] 
	where filterPrime (p:xs) = 
		p : filterPrime [x | x <- xs, x `mod` p /= 0]

doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2
double x = x * 2

let lostNumbers = [4,8,15,16,23,42]

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

---