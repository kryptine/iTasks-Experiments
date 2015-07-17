implementation module StdStringExt

import StdString

(<+) infixl 5 :: !String !a -> String | toString a
(<+) str a = str +++ toString a

(+>) infixl 5 :: !a !String -> String | toString a
(+>) a str = toString a +++ str
