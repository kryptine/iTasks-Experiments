definition module gameN

import iTasks

:: Game st = { game   :: String
             , state  :: [User] -> st
             , over   :: (Turn,st) -> Bool
             , winner :: (Turn,st) -> Task Turn
             , move   :: (Turn,st) -> Task st
             , board  :: (Turn,st) -> [HtmlTag]
             }
:: Turn    = { bound  :: !Int     // 0 < bound
             , current:: !Int     // 0 <= current < bound
             }
derive class iTask Game, Turn

new   :: !Int       -> Turn
next  ::      !Turn -> Turn
prev  ::      !Turn -> Turn
match :: !Int !Turn -> Bool

instance toInt Turn
instance ==    Turn

play_for_N :: !Int !(Game st) -> Task Turn | iTask st
