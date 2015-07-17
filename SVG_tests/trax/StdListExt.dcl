definition module StdListExt

import StdList

/** lookup @key @key_values = @values:
       returns all @values for which there is an occurrence of (@key,value) in @key_values.
*/
lookup :: !k ![(k,v)] -> [v] | Eq k

/** lookup1 @key @key_values = @value:
       returns the @value for which there is an occurrence of (@key,value) in @key_values.
       If this pair is missing, then lookup1 fails.
*/
lookup1 :: !k ![(k,v)] -> v | Eq k

/** spanfilter @cond @xs = (@yes,@no):
        @yes contains all elements of @xs for which @cond returns True, and
        @no  contains all elements of @xs for which @cond returns False.
*/
spanfilter :: !(a -> Bool) ![a] -> (![a],![a])

/** hasDup @as = True:
       @as has at least one element that occurs twice.
    hasDup @as = False:
       @as has no elements that occur twice.
*/
hasDup :: ![a] -> Bool | Eq a

/** removeMembersBy @select @as @bs = @as`:
       @as` is @as but with elements removed for which there exists a b in @bs such that
       (@select a b) holds.
*/
removeMembersBy :: !(a -> b -> Bool) ![a] ![b] -> [a]

// variants of the fold functions that use a queue instead of a fixed size list:
qfoldl :: (a -> b -> [b]) (a -> b -> a) a ![b] -> a
qfoldr :: (a -> b -> [b]) (b -> a -> a) a ![b] -> a

listToString :: ![a] -> String | toString a
