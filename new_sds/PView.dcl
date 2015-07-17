definition module PView

import StdEnv
import Data.Void, Data.Error, Data.Either, Text.JSON

:: *MyWorld 

createMyWorld :: *World -> *MyWorld
getWorld :: *MyWorld -> *World

// -----------------------------------------------------------------------

:: IFun p :== p -> Bool

:: Source p r w *env =
	{ get	:: p   env -> *(MaybeErrorString r, env)
	, put	:: p w env -> *(MaybeErrorString (IFun p), env)
	}

:: PView p` r` w` *env
	= 		 	Source 		(Source p` r` w` env)
	| E.r w:	Projection 	(PView  p` r  w  env) (Lens r w r` w`)
	// Isomorphism?!
	| E.p:		Translation (PView  p  r` w` env) (p` -> p) 							& TC p
	| E.p1 p2:	Split   	(PView  p1 r` w` env) (Split p2 r` w`) (p` -> (p1,p2)) 		& TC p1 & TC p2
	| E.p1 r1 w1 p2 r2 w2:
				Join		(PView  p1 r1 w1 env) (PView p2 r2 w2 env) (p` -> (p1,p2)) (w` -> (w1,w2)) (r1 r2 -> r`) & TC p1 & TC p2
	| E.p1 p2:
				Union		(PView  p1 r` w` env) (PView p2 r` w` env) (p` -> Either p1 p2) (p1 r` w` -> IFun p2) (p2 r` w` -> IFun p1) & TC p1 & TC p2
	// Arrow?
	| E.a r w p:
				PSeq		(PView p` a a env) (PView p r w env) (a -> p) (w` -> (a,w)) (a r -> r`) & TC p
	
	
:: View r w *env :== PView Void r w env

:: Lens r w r` w` =
	{ lget	:: r -> r`
	, lput	:: r w` -> w
	}

:: Split p r w = 
	{ sget	:: p r -> r
	, sput	:: p r w -> (w, IFun p)
	}

fixP 	  		 :: (PView p r w *env)	p 							-> View r w *env | TC p
applyLens 		 :: (PView p a a *env)	(Lens a a r` w`) 			-> PView p r` w` *env
applySplit 		 :: (PView p1 r w *env) (Split p2 r w) (p->(p1,p2)) -> (PView p r w *env) | TC p1 & TC p2
applyTranslation :: (PView p1 r w *env) (p2 -> p1) 					-> (PView p2 r w *env) | TC p1 & TC p2

join 	:: (PView p1 r1 w1 *env) (PView p2 r2 w2 *env) 										   -> (PView (p1,p2) (r1,r2) (w1,w2) *env) | TC p1 & TC p2
union 	:: (PView p1 r w *env)	 (PView p2 r w *env)   (p1 r w -> IFun p2) (p2 r w -> IFun p1) -> (PView (Either p1 p2) r w *env) | TC p1 & TC p2
pseq 	:: (PView p a a *env)	 (PView p` r2 w2 *env) (a -> p`) (w -> (a,w2)) (a r2 -> r) 	   -> (PView p r w *env) | TC p`

tr1 :: a -> (Void, a)
tr2 :: (a,b) -> (a,b)
tr3 :: (a,b,c) -> ((a,b),c)
tr4 :: (a,b,c,d) -> (((a,b),c),d)

// -----------------------------------------------------------------------

get :: (View r w *MyWorld) *MyWorld 	-> *(MaybeErrorString r, *MyWorld)
put :: (View r w *MyWorld) w *MyWorld 	-> *(MaybeErrorString Void, *MyWorld)

class registerForNotification env :: (PView p r w *env) p String *env -> *env | TC p
instance registerForNotification MyWorld

// -----------------------------------------------------------------------

createMemoryView 	:: a *MyWorld -> *(PView Void a a MyWorld, *MyWorld) | JSONEncode{|*|} a & JSONDecode{|*|} a
createStoreView 	:: String a -> (PView Void a a MyWorld) | JSONEncode{|*|} a & JSONDecode{|*|} a

