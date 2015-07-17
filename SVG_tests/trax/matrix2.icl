implementation module matrix2

import StdList

// a matrix2 of a is a list of rows of elements of type a
:: Matrix2 a :== [[a]]

dimension :: !(Matrix2 a) -> (!Int,!Int)
dimension m = (length (hd m), length m)

