primes = botler [2 .. ] where
    botler (hd : tl) = hd : [n | n <- tl, mod n hd /= 0]

fibs = 1 : 1 : sumLists fibs (tail fibs) where 
	sumLists (hd1 : tl1) (hd2 : tl2) = (hd1 + hd2) : sumLists tl1 tl2