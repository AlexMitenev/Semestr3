data BT a = N a (BT a) (BT a) | E deriving Show

insert E n = N n E E
insert (N a l r) n 
	| n < a = N a (insert l n) r
	| n > a = N a l (insert r n)
	| True = N a l r

find E _ = False
find (N a l r) n 
	| a == n = True
	| n > a = find r n
	| True = find l n

isBT E = True
isBT (N _ E E) = True
isBT (N n l@(N a _ _) r@(N b _ _))
	| n < a || n > b = False
	| True = isBT l && isBT r

elements E = []
elements (N a l r) = elements l ++ [a] ++ elements r

isEqual E E = True
isEqual (N n1 l1 r1) (N n2 l2 r2) = (n1 == n2)      && 
									 isEqual l1 l2  && 
									 isEqual r1 r2