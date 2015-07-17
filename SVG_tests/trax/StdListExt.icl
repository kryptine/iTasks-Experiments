implementation module StdListExt

import StdBool, StdList, StdStringExt

/** lookup @key @key_values = @values:
       returns all @values for which there is an occurrence of (@key,value) in @key_values.
*/
lookup :: !k ![(k,v)] -> [v] | Eq k
lookup key table
	= [v \\ (k,v) <- table | k == key]


/** lookup1 @key @key_values = @value:
       returns the @value for which there is an occurrence of (@key,value) in @key_values.
       If this pair is missing, then lookup1 fails.
*/
lookup1 :: !k ![(k,v)] -> v | Eq k
lookup1 key table
	= hd (lookup key table)

/** spanfilter @cond @xs = (@yes,@no):
        @yes contains all elements of @xs for which @cond returns True, and
        @no  contains all elements of @xs for which @cond returns False.
*/
spanfilter :: !(a -> Bool) ![a] -> (![a],![a])
spanfilter _ []
             = ([],[])
spanfilter cond [x:xs]
| cond x     = ([x:yes],no)
| otherwise  = (yes,[x:no])
where
	(yes,no) = spanfilter cond xs

/** hasDup @as = True:
       @as has at least one element that occurs twice.
    hasDup @as = False:
       @as has no elements that occur twice.
*/
hasDup :: ![a] -> Bool | Eq a
hasDup []     = False
hasDup [x:xs] = isMember x xs || hasDup xs

/** removeMembersBy @select @as @bs = @as`:
       @as` is @as but with elements removed for which there exists a b in @bs such that
       (@select a b) holds.
*/
removeMembersBy :: !(a -> b -> Bool) ![a] ![b] -> [a]
removeMembersBy select as bs
	= [a \\ a <- as | not (or [select a b \\ b <- bs])]

// variants of the fold functions that use a queue instead of a fixed size list:
qfoldl :: (a -> b -> [b]) (a -> b -> a) a ![b] -> a
qfoldl _ _ a []
	= a
qfoldl f g a [b:bs]
	= let a` = g a b in qfoldl f g a` (bs ++ f a` b)

qfoldr :: (a -> b -> [b]) (b -> a -> a) a ![b] -> a
qfoldr _ _ a []
	= a
qfoldr f g a [b:bs]
	= let a` = g b a in qfoldr f g a` (bs ++ f a` b)

listToString :: ![a] -> String | toString a
listToString []     = "[]"
listToString [x]    = "[" <+ x <+ "]"
listToString [x:xs] = "[" <+ x <+ elts xs
where
	elts []         = "]"
	elts [x:xs]     = "," <+ x <+ elts xs
