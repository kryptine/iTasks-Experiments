implementation module PView

import StdEnv, StdDebug
import Data.Void, Data.Tuple, Data.Error, Data.Func, Data.Either, Text.JSON, Data.List
import System.File, System.Time, Crypto.Hash.MD5
import graph_to_sapl_string

from Data.Map import :: Map
from Data.Set import :: Set
import qualified Data.Map as Map
import qualified Data.Set as Set

println :: !a !*MyWorld -> *MyWorld | toString a
println msg iw=:{world} 
	# (console,world)	= stdio world
	# console			= fwrites (toString msg) console
	# console			= fwrites "\n" console
	# (_,world)			= fclose console world
	= {iw & world = world} 

getDescriptor :: !a -> VIEWID
getDescriptor a = md5 (graph_to_sapl_string a)

geq a b = graph_to_sapl_string a == graph_to_sapl_string b

gt a b = trace_n (graph_to_sapl_string a) b 

genID world=:{nextid}
	= (nextid, {world & nextid = nextid + 1})
	
// -----------------------------------------------------------------------

:: VIEWID :== String

// Notificication request
:: NRequest = 
		{ nreqid  :: Int
		, target  :: VIEWID
		, param   :: Dynamic
		, handler :: *MyWorld -> *MyWorld
		}

/*
eventsource: The id of the source node of the write (top layer)
origin:      The id of the node of the original notification request
target:      The id of the node the notification is attached 

For the node you reqest for a notification "target == origin".
But there are many request generated automatically for the lower layers. For those "target <> origin"
A notification must be triggered:

viewid: the node we are currently processing

1. if the origin and eventsource is the same:

-----
| 1 | 
| 2 | <- eventsource, origin
| 3 |
| 4 |
| 5 |
-----

trigger: target == viewid && target == origin

2. if the origin is below eventsource in the graph

-----
| 1 | 
| 2 | <- eventsource
| 3 |
| 4 | <- origin
| 5 |
-----

trigger: target == viewid && target == origin

3. if eventsource is below the origin

-----
| 1 | 
| 2 | <- origin
| 3 |
| 4 | <- eventsource
| 5 |
-----

trigger: target == viewid && target == eventsource

UPDATE: counterexample (origin and eventsource are in two different branches)

-----
| 1 | 
| 2 | <- origin
| 3 |        ---
| 4 |--------|6] <- eventsource
| 5 |        --- 
-----

*/

:: *MyWorld = 
		{ sdsmem		:: Map Int JSONNode
        , sdsstore      :: Map String JSONNode
		, notification	:: [NRequest]
		, nextid		:: Int
		, world			:: *World
		}

createMyWorld :: *World -> *MyWorld
createMyWorld world = {MyWorld | sdsmem = 'Map'.newMap, sdsstore = 'Map'.newMap, notification = [], nextid = 1, world = world}

getWorld :: *MyWorld -> *World
getWorld {world} = world

// -----------------------------------------------------------------------

:: IFun p :== p -> Bool
:: IFunIO p *env :== p env -> *(Bool, env)

:: Source p r w *env =
	{ get	:: p   env -> *(MaybeErrorString r, env)
	, put	:: p w env -> *(MaybeErrorString (IFun p), env)
	}

:: Lens r w r` w` =
	{ lget	:: r -> r`
	, lput	:: r w` -> w
	}

:: Split p r w = 
	{ sget	:: p r -> r
	, sput	:: p r w -> (w, IFun p)
	}

applyLens :: (PView p a a *env) (Lens a a r` w`) -> PView p r` w` *env
applyLens pview lens = Projection pview lens

fixP :: (PView p r w *env) p -> View r w *env | TC p
fixP pview p = Translation pview (const p)

applySplit :: (PView p1 r w *env) (Split p2 r w) (p->(p1,p2)) -> (PView p r w *env) | TC p1 & TC p2
applySplit pview split tr = Split pview split tr

applyTranslation :: (PView p1 r w *env) (p2 -> p1) -> (PView p2 r w *env) | TC p1 & TC p2
applyTranslation pview tr = Translation pview tr

join :: (PView p1 r1 w1 *env) (PView p2 r2 w2 *env) -> (PView (p1,p2) (r1,r2) (w1,w2) *env) | TC p1 & TC p2
join pview1 pview2 = Join pview1 pview2 id id tuple

union :: (PView p1 r w *env) (PView p2 r w *env) (p1 r w -> IFun p2) (p2 r w -> IFun p1) -> (PView (Either p1 p2) r w *env) | TC p1 & TC p2
union pview1 pview2 ifun1 ifun2 = Union pview1 pview2 id ifun1 ifun2

pseq :: (PView p a a *env) (PView p` r2 w2 *env) (a -> p`) (w -> (a,w2)) (a r2 -> r) -> (PView p r w *env) | TC p`
pseq pview1 pview2 trp trw trr = PSeq pview1 pview2 trp trw trr

// These can be encoded as a type class if we have functional dependencies...
tr1 :: a -> (Void, a)
tr1 a = (Void, a)
tr2 :: (a,b) -> (a,b)
tr2 (a,b) = (a,b)
tr3 :: (a,b,c) -> ((a,b),c)
tr3 (a,b,c) = ((a,b), c)
tr4 :: (a,b,c,d) -> (((a,b),c),d)
tr4 (a,b,c,d) = (((a,b), c),d)

// -----------------------------------------------------------------------

get :: (View r w *MyWorld) *MyWorld -> *(MaybeErrorString r, *MyWorld)
get pview env = get` pview Void env

// TODO: return "read cache"
get` :: (PView p r w *env) p *env -> *(MaybeErrorString r, *env)
get` (Source {get}) p env 
	= get p env
	
get` (Projection pview` {lget}) p env
	# (ret, env) = get` pview` p env
	= (fmap lget ret, env)

get` (Translation pview` tr) p env
	= get` pview` (tr p) env

get` (Split pview` {sget} tr) p env
	# (p1, p2) = tr p
	# (ret, env) = get` pview` p1 env
	= (fmap (sget p2) ret, env)

get` (Join pview1 pview2 trp trw trr) p env
	# (p1, p2) = trp p
	# (ret1, env) = get` pview1 p1 env
	| isError ret1
		= (Error (fromError ret1), env)
	# (ret2, env) = get` pview2 p2 env
	| isError ret2
		= (Error (fromError ret2), env)
		= (Ok (trr (fromOk ret1) (fromOk ret2)), env)

get` (Union pview1 pview2 trp _ _) p env
	= case trp p of
		Left p1  = get` pview1 p1 env
		Right p2 = get` pview2 p2 env

get` (PSeq pview1 pview2 trp trw trr) p env
	= case get` pview1 p env of
		(Error msg, env) = (Error msg, env)
		(Ok r1, env) 	 = case get` pview2 (trp r1) env of
								(Error msg, env) = (Error msg, env)		
								(Ok r2, env)     = (Ok (trr r1 r2), env)

put :: (View r w *MyWorld) w *MyWorld -> *(MaybeErrorString Void, *MyWorld)
put pview w env 
	= case put` pview Void w env of
		(Error msg, _, myworld) = (Error msg, myworld)
		(Ok _, ns, myworld)
			# myworld = notificateAll ns myworld
			= (Ok Void, myworld)

// Notification event
:: NEvent = E.p: NEvent VIEWID (IFunIO p *MyWorld) & TC p 

nevent d ifun = NEvent (getDescriptor d) ifun
wrapIO ifun p env = (ifun p, env)

put` :: (PView p r w *MyWorld) p w *MyWorld -> (MaybeErrorString (IFunIO p *MyWorld), [NEvent], *MyWorld) | TC p
put` d=:(Source pview) p wval myworld
	= case pview.put p wval myworld of
		(Error msg, myworld) = (Error msg, [], myworld)
		(Ok ifun, myworld) 	 # ifun` = wrapIO ifun
		 					 = (Ok ifun`, [nevent d ifun`], myworld)

put` d=:(Projection pview {lput}) p wval env
	= case get` pview p env of 
		(Error msg, env) = (Error msg, [], env)
		(Ok rval, env)   = case put` pview p (lput rval wval) env of	
									(Error msg, _, env) = (Error msg, [], env)
									(Ok ifun, ns, env)  = (Ok ifun, [nevent d ifun: ns], env)

put` d=:(Translation pview tr) p wval env
	= case put` pview (tr p) wval env of
		(Error msg, _, env) = (Error msg, [], env)
		(Ok ifun, ns, env)  # ifun` = \p env -> ifun (tr p) env
							= (Ok ifun`, [nevent d ifun`: ns], env)

put` d=:(Split pview {sput} trp) p wval env 
	= case get` pview p1 env of 
		(Error msg, env) = (Error msg, [], env)
		(Ok rval, env)   # (wval, ifun2) = sput p2 rval wval
						 = case put` pview p1 wval env of	
								(Error msg, _, env) = (Error msg, [], env)
								(Ok ifun1, ns, env) # ifun` = genifun ifun1 (wrapIO ifun2)
													= (Ok ifun`, [nevent d ifun`: ns], env)
where
	genifun ifun1 ifun2 p env = genifun` ifun1 ifun2 (trp p) env // TODO: verify notification condition
	genifun` ifun1 ifun2 (p1`,p2`) env  
		| geq p1 p1` = ifun2 p2` env
					 = ifun1 p1` env
	(p1, p2) = trp p

put` d=:(Join pview1 pview2 trp trw _) p wval env 
	= case put` pview1 p1 w1 env of
		(Error msg, _, env)  = (Error msg, [], env)
		(Ok ifun1, ns1, env) = case put` pview2 p2 w2 env of
									(Error msg, _, env)  = (Error msg, [], env)
									(Ok ifun2, ns2, env) # ifun` = genifun ifun1 ifun2
														 = (Ok ifun`, [nevent d ifun`: ns1++ns2], env)
where
	genifun ifun1 ifun2 p env = genifun` ifun1 ifun2 (trp p) env
	genifun` ifun1 ifun2 (p1,p2) env 
		= case ifun1 p1 env of
			(False, env) = ifun2 p2 env 
			(True, env)  = (True, env)
			
	(p1, p2) = trp p
	(w1, w2) = trw wval

put` d=:(Union pview1 pview2 trp ifunl ifunr) p wval env
	= case trp p of
	
		Left p1 = case get` pview1 p1 env of
					(Error msg, env) = (Error msg, [], env)
					(Ok rval, env)   = case put` pview1 p1 wval env of
											(Error msg, _, env) = (Error msg, [], env)
											(Ok ifun, ns, env)  # ifun` = genifun (wrapIO (ifunl p1 rval wval)) ifun 
 														   		= (Ok ifun`, [nevent d ifun`: ns], env)

		Right p2 = case get` pview2 p2 env of
					(Error msg, env) = (Error msg, [], env)
					(Ok rval, env)   = case put` pview2 p2 wval env of
											(Error msg, _, env) = (Error msg, [], env)
											(Ok ifun, ns, env) # ifun` = genifun ifun (wrapIO (ifunr p2 rval wval))
 															   = (Ok ifun`, [nevent d ifun`: ns], env)
where
	genifun ifun1 ifun2 p env = case trp p of
								(Left p1)  = ifun2 p1 env
								(Right p2) = ifun1 p2 env

put` d=:(PSeq pview1 pview2 trp trw trr) p wval env
	# (w1, w2) = trw wval
	= case put` pview1 p w1 env of
			(Error msg, _, env)  = (Error msg, [], env)
			(Ok ifun1, ns1, env) = case put` pview2 (trp w1) w2 env of
									(Error msg, _, env)  = (Error msg, [], env)
									(Ok ifun2, ns2, env) # ifun` = genifun ifun1 ifun2
													     = (Ok ifun`, [nevent d ifun`: ns1++ns2], env)  
where
	genifun ifun1 ifun2 p env 
		= case ifun1 p env of
			(False, env) = case get` pview1 p env of
								(Error msg, env) = (True, env)
								(Ok rval, env)   = ifun2 (trp rval) env
			(True, env)  = (True, env)

notificateAll :: [NEvent] *MyWorld -> *MyWorld
notificateAll ns myworld=:{MyWorld|notification} 
	# (myworld,_,ds) = foldl notificate (myworld,'Set'.newSet,[]) ns
	= {myworld & notification = filter (\{nreqid} -> not (isMember nreqid ds)) notification}
where
	notificate st (NEvent viewid invalidator)  
			= foldl geninv st notification
	where
		geninv (myworld,s,ds) {nreqid, target, handler, param} | target == viewid && 'Set'.notMember nreqid s
			= case param of
				(qr :: qr^) = case invalidator qr myworld of
									(False, myworld) = (myworld, 'Set'.insert nreqid s, ds)
									(True, myworld)  = (handler myworld, 'Set'.insert nreqid s, [nreqid:ds])
							= (myworld, 'Set'.insert nreqid s, ds)
			
		geninv a _ = a

class registerForNotification env :: (PView p r w *env) p String *env -> *env | TC p

instance registerForNotification MyWorld
where
	registerForNotification pview p msg myworld=:{MyWorld|notification}
		# (nreqid, myworld)   = genID myworld
		# (lowerids, myworld) = collectIds pview p myworld
		# lowerLayers = [createRequest viewid dynparam nreqid \\ (viewid, dynparam) <- lowerids]
		= {MyWorld | myworld & notification = [createRequest (getDescriptor pview) (dynamic p) nreqid:tl lowerLayers ++ notification]}
	where	
		createRequest target param nreqid = 	
										{ nreqid = nreqid
										, target = target
										, param = param
										, handler = println ("Notification: "+++msg)
										} 
	
		collectIds :: (PView p r w *MyWorld) p *MyWorld -> *([(VIEWID, Dynamic)], *MyWorld) | TC p
		collectIds d=:(Source _) p myworld = ([(getDescriptor d, dynamic p)], myworld)
		collectIds d=:(Projection pview _) p myworld
				= let (ids, myworld`) = collectIds pview p myworld in ([(getDescriptor d, dynamic p):ids], myworld`)
		collectIds d=:(Translation pview tr) p myworld
				= let (ids, myworld`) = collectIds pview (tr p) myworld in ([(getDescriptor d, dynamic p):ids], myworld`)		
		collectIds d=:(Split pview split trp) p myworld
				= let (ids, myworld`) = collectIds pview (fst (trp p)) myworld in ([(getDescriptor d, dynamic p):ids], myworld`)					

		collectIds d=:(Join pview1 pview2 trp _ _) p myworld
				# (ids1, myworld) = collectIds pview1 p1 myworld
				# (ids2, myworld) = collectIds pview2 p2 myworld
				= ([(getDescriptor d, dynamic p):ids1 ++ ids2], myworld)
		where
			(p1, p2) = trp p
			
		collectIds d=:(Union pview1 pview2 trp _ _) p myworld
			= case trp p of
				Left p1  = let (ids, myworld`) = collectIds pview1 p1 myworld in ([(getDescriptor d, dynamic p):ids], myworld`)
				Right p2 = let (ids, myworld`) = collectIds pview2 p2 myworld in ([(getDescriptor d, dynamic p):ids], myworld`)				
						
		collectIds d=:(PSeq pview1 pview2 trp _ _) p myworld
				# (ids1, myworld) = collectIds pview1 p myworld
				# (Ok rval, myworld) = get` pview1 p myworld
				# (ids2, myworld) = collectIds pview2 (trp rval) myworld				
				= ([(getDescriptor d, dynamic p):ids1 ++ ids2], myworld)

// -----------------------------------------------------------------------

createMemoryView :: a *MyWorld -> *(PView Void a a MyWorld, *MyWorld) | JSONEncode{|*|} a & JSONDecode{|*|} a
createMemoryView val myworld=:{sdsmem}
	# (memid, myworld) = genID myworld
	# sds = Source {Source | get  = get` memid, put = put` memid}
	= (sds, {myworld & sdsmem = 'Map'.put memid (toJSON val) sdsmem})
where
	get` memid Void myworld=:{sdsmem}
		= case 'Map'.get memid sdsmem of
			(Just json) = case fromJSON json of
								(Just val) = (Ok val, myworld)
								Nothing	   = (Error "E2", myworld)
			Nothing 	= (Error "E1", myworld)

	put` memid Void val myworld=:{sdsmem}
		= (Ok (const True), {myworld & sdsmem = 'Map'.put memid (toJSON val) sdsmem})

createStoreView :: String a -> (PView Void a a MyWorld) | JSONEncode{|*|} a & JSONDecode{|*|} a
createStoreView name def = Source {Source | get = get` name def, put = put` name}
where
    get` name def Void myworld=:{sdsstore}
        = case 'Map'.get name sdsstore of
			(Just json) = case fromJSON json of
								(Just val) = (Ok val, myworld)
								Nothing	   = (Error "E2", myworld)
			Nothing 	= (Ok def, myworld)

    put` name Void val myworld=:{sdsstore}
        = (Ok (const True), {myworld & sdsstore = 'Map'.put name (toJSON val) sdsstore})


	
	
	
	
	
	
	
