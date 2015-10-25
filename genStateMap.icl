module genStateMap

import StdEnv, StdGeneric, GenMap, Data.Maybe

derive gStMap (,), [], Tree, T, Maybe

// gStMap, this one goes from left to right... we might also want other ones...

generic gStMap a b :: (s,a) -> (s,b)

gStMap{|c|}    id = id
gStMap{|UNIT|} id = id

gStMap{|PAIR|} fx fy (s,PAIR x y)	
# (s,nx) = fx (s,x) 
# (s,ny) = fy (s,y)
= (s,PAIR nx ny)  

gStMap{|EITHER|} fl fr (s,LEFT x) 	
# (s,nx) = fl (s,x) 
= (s,LEFT nx)
gStMap{|EITHER|} fl fr (s,RIGHT x) 	
# (s,nx) = fr (s,x) 
= (s,RIGHT nx)

gStMap{|CONS|} f (s,CONS x) 		
# (s,nx) = f(s,x)
= (s,CONS nx)

gStMap{|OBJECT|} f (s,OBJECT x) 		
# (s,nx) = f (s,x)
= (s,OBJECT nx)

// with gStMap arbitrary values in a given data structures can be changed

updStS :: (Int,a) (t a) -> (t a) | gStMap{|*->*|} t
updStS (i,e) sa = snd (gStMap{|*->*|} upd (i,sa))
where
	upd (0,s) = (-1,e) 
	upd (i,s) = (i-1,s)

updStStS :: ((Int,a),(Int,b)) (t a b) -> (t a b) | gStMap{|*->*->*|} t
updStStS ((i,a),(j,b)) sab = snd (gStMap{|*->*->*|} upda updb ((i,j),sab))
where
	upda ((0,j),s) = ((-1,j),a) 
	upda ((i,j),s) = ((i-1,j),s)

	updb ((i,0),s) = ((i,-1),b) 
	updb ((i,j),s) = ((i,j-1),s)

getStS :: Int (t a) -> Maybe a | gStMap{|*->*|} t
getStS i sa = (snd (fst (gStMap{|*->*|} get ((i,Nothing),sa))))
where
	get ((0,_),e) = ((-1,Just e),e)
	get ((i,f),e) = ((i-1,f),e)

exchStS :: (Int,Int) (t a) -> (Maybe (a,a),t a) | gStMap{|*->*|} t
exchStS (i,j) sa 
# mbie = getStS i sa
| isNothing mbie = (Nothing,sa)
# mbje = getStS j sa
| isNothing mbje = (Nothing,sa)
# ie = fromJust mbie
# je = fromJust mbje
# sa = updStS (i,je) sa
# sa = updStS (j,ie) sa
= (Just (ie,je),sa)

updStSPred :: (a -> Maybe a) (t a) -> (t a) | gStMap{|*->*|} t
updStSPred fmba sa = snd (gStMap{|*->*|} change (undef,sa))
where
	change (u,a) 
	# mba = fmba a 
	| isNothing mba = (u,a) 
	= (u,fromJust mba)

// other examples

maxStS :: (t a) -> (Maybe a) | gStMap{|*->*|} t & Ord a
maxStS ta = fst (gStMap{|*->*|} max (Nothing,ta))
where
	max (Nothing,a) = (Just a,a)
	max (Just m,a) 
	| a > m = (Just a,a)
	= (Just m,a) 

foldlStS :: (b a -> b) b (t a) -> b | gStMap{|*->*|} t
foldlStS op e ta = fst (gStMap{|*->*|} fold (e,ta))
where
		fold (b,a) = (op b a,a)
	


//
:: Tree a = Node a (Tree a) (Tree a) | Leaf

myTree :: Tree Int
myTree = toNode [1..10]

toNode [] 		= Leaf
toNode [x:xs]	= Node x (toNode ls) (toNode rs)
where
	(ls,rs) = splitAt (length xs / 2) xs

:: Tree2 a b = Node2 a (Tree2 a b) (Tree2 a b) | Leaf2 b

toNode2 [] 		= Leaf2 0
toNode2 [x:xs]	= Node2 x (toNode2 ls) (toNode2 rs)
where
	(ls,rs) = splitAt (length xs / 2) xs


:: T a b = T a b


//Start = updStStS ((0,12),(0,13)) (1,2)
//Start = updStSPred (\a -> (if (isEven a) (Just (a+1)) Nothing)) [1..10]
//Start = updStS (4,44) [1..10]			
//Start = updStS (3,44) myTree			
//Start = getStS 0 (1,2)			
//Start = getStS 5 [1..10]			
//Start = exchStS (3,4) [1..10]
//Start = exchStS (2,4) (toNode [1..10])
//Start = maxStS [1..10]
Start = foldlStS (+) 0 [1..10]






