type Parser a = String -> [(a, String)]


data E = X String | N Integer | Mul E E |
		 Div E E  | Add E E   | Sub E E |
		 Eq  E E  | Geq E E   | Leq E E |
		 Le  E E  | Gr  E E   | Neq E E |
		 And E E  | Or  E E  deriving Show

--Parser functions

empty :: Parser a
empty _ = []

sym :: Char -> Parser Char
sym c (x:xs) | x == c = [(c, xs)] 
sym c _               = []

val :: a -> Parser a
val a s = [(a, s)]

infixl 2 |||
(|||) :: Parser a -> Parser a -> Parser a
p ||| q = \ s -> p s ++ q s

infixl 3 ||>
(||>) :: Parser a -> (a -> Parser b) -> Parser b
p ||> q = \s -> concat [q a s | (a, s) <- p s] 

many :: Parser a -> Parser [a]
many a = a ||> (\ x -> many a ||> val . (x:)) ||| 
         val []

opt :: Parser a -> Parser (Maybe a)
opt a = a ||> val . Just ||| val Nothing

eof :: [(a, String)] -> [a]
eof = map fst . filter ((==[]) . snd) 

--Grammar

oneOf = foldl (\ a b -> a ||| sym b) empty 


letter = oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
digit = oneOf "0123456789"

ident   = letter ||> (\ x -> many (letter ||| digit) ||> (\ xs -> val $ X (x:xs)))

literal = digit ||> (\ x -> many digit ||> (\ xs -> val $ N $ read (x:xs)))

primary = 
    ident   ||| 
    literal ||| 
    sym '(' ||> (\ _ -> expr ||> (\ e -> sym ')' ||> (\ _ -> val e)))

makeGrammar member operList = member ||> (\ fstArg -> (opAndArgsList ||> (\args -> val (foldl oneOp fstArg args))))
	where
		opAndArgsList = many (op ||> (\ o -> member ||> (\arg -> val (o, arg))))
		op = makeOpList operList
		oneOp res (o, arg) = (o) res arg


makeNonAssoc member operList = member ||> (\ x -> op ||> (\ o -> member ||> (\y -> val $ x `o` y))) ||| member
	where
		op = makeOpList operList

makeOpList  operList = 
	foldl (|||) (head listOp) (tail listOp)
		where listOp = (map (\ (o, c) -> makeOp o c) operList)

makeOp (c : chars) constr
	| chars == [] = sym c ||> (\ _ -> val constr)
	| otherwise   = sym c ||> (\ _ -> sym (head chars) ||> (\ _ -> val constr))

multi = makeGrammar primary op
	where op = [("*", Mul), ("/", Div)]

addi = makeGrammar multi op
	where op = [("+", Add), ("-", Sub)]

reli = makeNonAssoc addi op
	where op = [("=",  Eq), ("<",  Le), (">",  Gr), (">=", Geq), ("<=", Leq), ("!=", Neq)]  

logi = makeGrammar reli op
	where op = [("&", And), ("|", Or)]

expr = logi