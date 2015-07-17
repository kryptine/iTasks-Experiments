definition module StdStringExt

import StdString

(<+) infixl 5 :: !String !a -> String | toString a
(+>) infixl 5 :: !a !String -> String | toString a
