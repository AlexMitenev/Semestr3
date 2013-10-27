data M a b = E | M  Integer (a, [b]) (M a b) (M a b) deriving Show

--support functions
h E            = 0
h (M n _ _ _ ) = n
make a l r = M ( max (h l) (h r) + 1) a l r
val (M _ n _ _) = n

balanse E = E
balanse m@(M  heigh  pair@(key, values)  l  r)
	| h l >  h r + 1 = rotR pair l r
	| h l == h r + 1 = make pair l r
	| h r >  h l + 1 = rotL pair l r
	| h r == h l + 1 = make pair l r
	|otherwise = m
	where
		rotL a l (M _ c rl rr) = if h rl > h rr then rotL a l (rotR c rl rr)
						               		    else make c ( make a l rl) rr

		rotR a (M _ c ll lr) r = if h lr > h ll then rotR a (rotL c ll lr) r
							                    else make c ll ( make a lr r)
--map functions
empty = E 

find E _ = Nothing
find (M _ (key, v : vals) l r) k
	|k < key = find l k
	|k > key = find r k
	|otherwise = Just v

insert E (k, v) = M 1 (k, [v]) E E
insert (M  heigh  pair@(key, values)  l  r) new@(k, v)
	| k < key = let l' = insert l new in
		balanse (M  heigh (key, values)  l'  r)
	| k > key = let r' = insert r new in
		balanse (M  heigh (key, values)  l  r')
	|otherwise = M heigh (key, v : values) l r

remove E _ = error "no key for remove"
remove m@(M  heigh  pair@(key, values)  l  r) k
	|k < key = balanse $ make pair (remove l k) r
	|k > key = balanse $ make pair (remove r k) l
	|length values > 1 = (M heigh (key, tail values)  l  r)
	|otherwise = remElem m
	where
		remElem (M _ _ E E) = E
		remElem (M _ _ l E) = l
		remElem (M _ _ E r) = r
		remElem (M _ _ l r) = let r' = remElem minE in  make (val minE) l r'
			where
				minE = minElem r
					where
						minElem m@( M _ n E E ) = m
						minElem ( M _ _ l _ )   = minElem l

test = foldl insert E [(1,2),(1,3),(2,2),(5,3),(8,9), (10,5)]

fold _ E acc              = acc
fold f ( M _ (k, v) l r ) acc  = fold f l (map f v : (fold f r acc))

