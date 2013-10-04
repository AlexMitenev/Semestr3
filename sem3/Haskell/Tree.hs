data BT a = N a (BT a) (BT a) | E deriving Show

myTree = (N 4 (N 3 E E) (N 5 E E))

insert E n = N n E E
insert (N a l r) n 
	| n < a = N a (insert l n) r
	| n > a = N a l (insert r n)
	| True = N a l r

find E n = False
find (N a l r) n 
	| a == n = True
	| n > a = find r n
	| n < a = find l n

isBT E = True
isBT (N n E E) = True
isBT (N n l@(N a _ _) r@(N b _ _))
	| n < a || n > b = False
	| True = isBT l && isBT r

elements E = []
elements (N a l r) = elements l ++ [a] ++ elements r