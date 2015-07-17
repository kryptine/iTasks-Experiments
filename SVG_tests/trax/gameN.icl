implementation module gameN

import StdMisc
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

new :: !Int -> Turn
new bound = if (bound <= 0) (abort ("new turn not applied to positive argument (" <+++ bound <+++ ")"))
                            {bound = bound, current = 0}

next :: !Turn -> Turn
next turn=:{current,bound} = {turn & current = (current+1) rem bound}

prev :: !Turn -> Turn
prev turn=:{current,bound} = {turn & current = (bound+current-1) rem bound}

match :: !Int !Turn -> Bool
match nr turn              = nr == turn.current

instance toInt Turn where toInt turn = turn.current
instance ==    Turn where == t1 t2 = t1 === t2

play_for_N :: !Int !(Game st) -> Task Turn | iTask st
play_for_N n game
	=             get_players n
      >>= \all -> withShared (new n,game.Game.state all)
         (\sharedGameSt -> anyTask [  user @: play_for_1 game nr sharedGameSt
                                   \\ user <- all & nr <- [0..]
                                   ])

get_players :: !Int -> Task [User]
get_players n
	= enterSharedMultipleChoice ("Select " <+++ max 0 n <+++ " players") [] users
	  >>* [ OnValue (ifValue (\selection -> length selection == max 0 n) return)
	      , OnAction ActionCancel (always (throw "Selection of players cancelled."))
	      ]

play_for_1 :: !(Game st) !Int !(Shared (Turn,st)) -> Task Turn | iTask st
play_for_1 game my_turn sharedGameSt
    = gaze ||- play
where
    gaze = viewSharedInformation game.game [ ViewWith game.board ] sharedGameSt
    play = watch sharedGameSt
           >>* [ OnValue (ifValue game.over game.winner)
               , OnValue (ifValue (\(turn,_)  -> match my_turn turn)
                                  (\(turn,st) ->         game.move (turn,st)
                                              >>= \st -> set (next turn,st) sharedGameSt
                                              >>| play
                                  )
                         )
               ]
