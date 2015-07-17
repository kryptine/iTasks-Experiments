definition module StdTupleExt

import StdTuple

class fromTuple a b c :: !(!a,!b) -> c
class toTuple   a b c :: !c -> (!a,!b)

dup2 :: a -> .(a,a)
