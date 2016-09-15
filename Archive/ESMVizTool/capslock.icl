module capslock

import ESMVizTool
import StdMisc

Start :: *World -> *World
Start world	= esmVizTool capslockESM world

:: State = Off | On
:: Input = Key Char | CapsLock //| ShiftDown | ShiftUp
:: Output :== Char

model :: (State,Input) -> ([Output],State)
model (Off, CapsLock) = ([], On)
model (On , CapsLock) = ([], Off)
model (Off, Key c)    = ([toLower c], Off)
//model (On , Key c)    = ([toUpper c], On)
model (On , Key c)
	| isAlpha c
		= ([toUpper c], On)
		= ([c], Off)

capslockESM :: ESM State Input Output
capslockESM =
	{ s_0 = Off
	, d_F = liftModel model
	, out = undef
	, pred = healthy
	, esm_name = "Capslock ESM"
	}

liftModel :: ((s,i) -> ([o],s)) -> Spec s i o
liftModel m = \s i.let (o,t) = m (s,i) in [Pt o t]

healthy :: (SeenTrans State Input Output) -> [[String]]
healthy (s,i,o,t) = []

instance == State where (==) s t = s === t
derive class iTask  State, Input
derive ggen Input
derive bimap []
derive genShow State, Input

instance render State	where render state	= show1 state
instance render Input   where render i		= show1 i
instance render Output  where render o		= show1 o
