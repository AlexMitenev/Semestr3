reduce (a, 0) = error "div by zero"
reduce (a, b) = (a `div` x, b `div` x) where
	x = (gcd a b)


(a, 0) \* _ = error "div by zero"
_ \* (c, 0) = error "div by zero"
(a, b) \* (c, d) = reduce (a*c, b*d)

(a, 0) \\ _ = error "div by zero"
_ \\ (c, 0) = error "div by zero"
(a, b) \\ (c, d) = reduce (a*d, b*c)

(a, 0) \+ _ = error "div by zero"
_ \+ (c, 0) = error "div by zero"
(a, b) \+ (c, d) = reduce (a * d + b * c, b*d)

(a, 0) \- _ = error "div by zero"
_ \- (c, 0) = error "div by zero"
(a, b) \- (c, d) = reduce (a * d - b * c, b*d)

infixr 6 \+
infixr 6 \-
infixr 7 \\
infixr 7 \*

[] !+ pal = pal
pal !+ [] = pal
pal1@((n1, p1) : tl1) !+ pal2@((n2, p2) : tl2) = normal list where
	list
	    |p1 == p2 = (n1 \+ n2, p1) : (tl1 !+ tl2)
	    |p1 > p2 = (n1, p1) : tl1 !+ pal1
	    |p1 < p2 = (n2, p2) : pal2 !+ tl2

[] !- pal = pal !* [((-1,1), 0)]
pal !- [] = pal
pal1@((n1, p1) : tl1) !- pal2@((n2, p2) : tl2) = normal list where
	list
	    |p1 == p2 = (n1 \- n2, p1) : (tl1 !- tl2)
	    |p1 > p2 = (n1, p1) : tl1 !- pal2
	    |p1 < p2 = (n2, p2) : pal1 !- tl2


normal [] = []
normal (hd : tl) = listSort (removeZero (reduceOne hd (normal tl))) where

	reduceOne (n, p) [] = [(n, p)]
	reduceOne (n, p) ((n1, p1) : tl)
		| p == p1 = (n \+ n1, p) : tl
		| True = (n1,p1) : (reduceOne (n, p) tl)

	removeZero l = [(n1,p1)|(n1,p1) <- l, n1 /= (0,1)]

	listSort [] = []
	listSort ((n, p):tl) = listSort [(n1,p1)|(n1,p1) <- tl, p1 > p] ++
	                       [(n, p)] ++ 
	                       listSort [(n1,p1)|(n1,p1) <- tl,p1 <= p]

[] !* _ = []
_ !* [] = [] 
pal1@((n1,p1) : tl) !* pal2@((n2,p2) : tl2) = normal l where 
	l = monom ++ tl !* pal2 where
			monom = mulOne (n1, p1) pal2 where
				mulOne _ [] = []
				mulOne (n1, p1) ((n2, p2) : tl2) =  (n1 \* n2, p1 + p2) : mulOne (n1, p1) tl2

_ !/ [] = error "divizion by zero"
[] !/ (h:t) = []
pal1@((n1,p1) : tl) !/ pal2@((n2,p2) : tl2)
		| p1 >= p2 = ((n1 \\ n2), p1 - p2 ) : (pal1 !- [((n1 \\ n2), p1 - p2)] !* pal2) !/ pal2
		| True = []

_ !% [] = error "divizion by zero"
[] !% (h:t) = []
pal1@((n1,p1) : tl) !% pal2@((n2,p2) : tl2)
		| p1 >= p2 =  (pal1 !- [((n1 \\ n2), p1 - p2)] !* pal2) !% pal2
		| True = pal1

gcdPals l1 [] = l1
gcdPals l1 l2 = gcdPals l2 (l1 !% l2)

infixr 6 !+
infixr 6 !-
infixr 7 !*
infixr 7 !/
infixr 7 !%