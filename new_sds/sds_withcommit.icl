module sds

import StdEnv
import Data.Void, Data.Error, Data.Map, Data.Func, Text.JSON
import System.File, System.Time 

println :: !a !*MyWorld -> *MyWorld | toString a
println msg iw=:{world} 
	# (console,world)	= stdio world
	# console			= fwrites (toString msg) console
	# console			= fwrites "\n" console
	# (_,world)			= fclose console world
	= {iw & world = world} 

getDescriptor :: !a -> SDSID
getDescriptor _
	= code {
		pushD_a	0
		pop_a	1
	}

genID world=:{nextid}
	= (nextid, {world & nextid = nextid + 1})

// -----------------------------------------------------------------------

:: SDSID :== Int

:: NotificationRequest = 
		{ target  :: SDSID
		, origin  :: SDSID
		, request :: IRequest
		, handler :: *MyWorld -> *MyWorld
		}

:: *MyWorld = 
		{ sdsmem		:: Map Int JSONNode
		, notification	:: [NotificationRequest]
		, nextid		:: Int
		, world			:: *World
		}

createMyWorld :: *World -> *MyWorld
createMyWorld world = {MyWorld | sdsmem = newMap, notification = [], nextid = 1, world = world}

:: InvalidateFun q :== q -> Bool // TODO: InvalidateFun q :== [q] -> [Int]
:: CommitFun *env :== env -> env 
:: AbortFun *env :== env -> env

:: IRequest = //IView Dynamic
			  E.qr: IView qr (Dynamic -> (qr -> Bool))
		    | IAlways

:: WriteRes q *env = 
		{ invalidate	:: q -> Bool
		, commit 		:: env -> env
		, abort  		:: env -> env
		}

:: ChangeNotification	= OnWrite
						| Predictable
						| Polling !Timestamp

:: Shared qr qw r w *env
	= 					Source  		(Source qr qw r w env)
	| E.qr` qw` r` w`:	Projection 		(Shared qr` qw` r` w` env) 
										(Projection qr` qw` r` w` qr qw r w) & TC qr`
	
	| E.qr` r`:			ComposedRead 	(Shared qr` qw  r` w  env) (qr -> (qr`,  r` -> r)) & TC qr`
	| E.qw` w`:			ComposedWrite 	(Shared qr  qw` r  w` env) 						  (qw -> (qr,  r  w -> (qw`, w`, InvalidateFun qr)))
	| E.qr` qw` r` w`:	ComposedRW		(Shared qr` qw` r` w` env) (qr -> (qr`, r` -> r)) (qw -> (qr`, r` w -> (qw`, w`, InvalidateFun qr))) & TC qr`
	| E.qrx qwx rx wx qry qwy ry wy:
		Parallel (Shared qrx qwx rx wx env) (Shared qry qwy ry wy env) (qr->(qrx,qry)) (qw->(qwx,qwy)) ((rx,ry)->r) (w->(wx,wy)) & TC qrx & TC qry

:: Source qr qw r w *env =
	{ read 			:: qr   env	-> *(MaybeErrorString r, env)   // Valid for ...
	, write 		:: qw w env -> *(MaybeErrorString (WriteRes qr env), env)
	}

:: Projection qr` qw` r` w` qr qw r w =
	{ pr_read   :: (qr     -> (qr`, r` -> r))
	, pr_write  :: (qw     -> (qr`, r` w -> (qw`, w`)))
	, pr_ntfcnt :: (qw  w  -> InvalidateFun qr)
	, pr_inv    :: (qw` w` -> (qw, w))
	}

:: SymmetricShared q r env :== Shared q q r r env

simpleRW = (\Void -> (Void, id), \Void -> (Nothing, (\Nothing v -> (Void, v, const True))))

// A.K.A withShared
createMemorySDS :: (q -> (Void, r -> r), 
					q -> (Maybe Void, (Maybe r) r -> (Void, r, InvalidateFun q))) 
				   r
				   *MyWorld 
		-> *(SymmetricShared q r MyWorld, *MyWorld) | JSONEncode{|*|} r & JSONDecode{|*|} r
createMemorySDS (rt,wt) val myworld=:{sdsmem}
	# (memid, myworld) = genID myworld
	# sds = Source {read  = read` memid rt, write = write` memid wt}
	= (sds, {myworld & sdsmem = put memid (toJSON val) sdsmem})
where
	readAll :: Int *MyWorld -> *(MaybeErrorString r, *MyWorld) | JSONDecode{|*|} r
	readAll memid myworld=:{sdsmem} 
		= case get memid sdsmem of
			(Just json) = case fromJSON json of
								(Just val) = (Ok val, myworld)
								Nothing	   = (Error "E2", myworld)
			Nothing 	 = (Error "E1", myworld)

	read` memid rt q myworld=:{sdsmem}
		# (ret, myworld) = readAll memid myworld
		# (_, tf) = rt q
		= (fmap tf ret, myworld)

	write` memid wt q val myworld=:{sdsmem}
		# (mbRead, tf) = wt q
		= case mbRead of
			(Just Void) # (ret, myworld) = readAll memid myworld
						= case ret of
							(Error msg) 	= (Error msg, myworld)
							(Ok full)		= let (_, nval, invalidator) = tf (Just full) val
										   	   in (Ok {invalidate=invalidator, commit=commit memid (toJSON nval), abort=id}, myworld)

			Nothing		= let (_, nval, invalidator) = tf Nothing val
						   in (Ok {invalidate=invalidator, commit=commit memid (toJSON nval), abort=id}, myworld)
		where
			commit memid json myworld=:{sdsmem} 
				= {myworld & sdsmem = put memid json sdsmem}
		
read :: (Shared qr qw r w *MyWorld) qr *MyWorld -> *(MaybeErrorString r, *MyWorld) | TC qr
read (Source {read}) q myworld
	# (ret, myworld) = read q myworld
	= (ret, myworld)

read (Projection sds pr) q myworld
	# (oq, f) = pr.pr_read q
	# (ret, myworld) = read sds oq myworld
	= case ret of 
		(Error msg)	= (Error msg, myworld)
		(Ok val)	= (Ok (f val), myworld)

read (ComposedRead sds tf) q myworld
	= read (ComposedRW sds tf undef) q myworld

read (ComposedRW sds tfr _) q myworld 
	# (oq, f) = tfr q
	# (ret, myworld) = read sds oq myworld
	= case ret of 
		(Error msg)	= (Error msg, myworld)
		(Ok val)	= (Ok (f val), myworld)

read (ComposedWrite sds tf) q myworld 
	= read sds q myworld

read (Parallel sdsX sdsY tfqr _ tfr _) q myworld 
	# (qrx,qry) = tfqr q
	# (ret, myworld) = read sdsX qrx myworld
	= case ret of 
		(Error msg)	= (Error msg, myworld)
		(Ok val1)	# (ret, myworld) = read sdsY qry myworld
					= case ret of 		
						(Error msg)	= (Error msg, myworld)					
						(Ok val2)   = (Ok (tfr (val1,val2)), myworld)

write :: (Shared qr qw r w *MyWorld) qw w *MyWorld -> *(MaybeErrorString Void, *MyWorld) | TC qr
write sds qw val myworld
	= case write` sds qw val myworld of
		(Error msg, ts, _, _, myworld) = (Error msg, foldl (flip ($)) myworld (map snd ts))
		(Ok Void, ts, ns, _, myworld) 
			# myworld = foldl (flip ($)) myworld (map fst ts)
			# myworld = notificateAll (getDescriptor sds) ns myworld
			= (Ok Void, myworld)
		
write` :: (Shared qr qw r w *MyWorld) qw w *MyWorld -> (MaybeErrorString Void, [(*MyWorld -> *MyWorld, *MyWorld -> *MyWorld)], [NotificationEvent], *MyWorld) | TC qr
write` d=:(Source sds) qw val myworld
	= case sds.write qw val myworld of
		(Error msg, myworld) 	= (Error msg, [], [], myworld)
		(Ok r, myworld) 		= (Ok Void, [(r.commit,r.abort)], [NotificationEvent (getDescriptor d) (qw, val)], myworld)

write` d=:(Projection sds pr) qw val myworld
	# (qr, f) = pr.pr_write qw
	# (ret, myworld) = read sds qr myworld
	= case ret of 
		(Error msg) = (Error msg, [], [], undef, myworld)
		(Ok rval)  	# (oqw, oval) = f rval val
					= case write` sds oqw oval myworld of	
						(Error msg, ts, _, _, myworld) 	= (Error msg, ts, [], myworld)
						(Ok Void, ts, ns, _, myworld)  	= (Ok Void, ts, [NotificationEvent (getDescriptor d) (qw, val):ns], myworld)

write` d=:(ComposedWrite sds tf) qw val myworld
	# (ret, ts, ns, inv, myworld) = write` (ComposedRW sds (\a -> (a, id)) tf) qw val myworld
	| isError ret
		= (ret, ts, ns, myworld)
		= (ret, ts, [NotificationEvent (getDescriptor d) (qw, val):ns], myworld)

write` d=:(ComposedRW sds _ tf) qw val myworld
	# (qr, f) = tf qw
	# (ret, myworld) = read sds qr myworld
	= case ret of 
		(Error msg) = (Error msg, [], [], undef, myworld)
		(Ok mbVal)  # (oqw, oval, invalidator) = f mbVal val
					= case write` sds oqw oval myworld of	
						(Error msg, ts, _, _, myworld) 	= (Error msg, ts, [], myworld)
						(Ok Void, ts, ns, _, myworld)  	= (Ok Void, ts, [NotificationEvent (getDescriptor d) (qw, val):ns], myworld)

write` d=:(ComposedRead sds tf) qw val myworld
	# (ret, ts, ns, inv, myworld) = write` sds qw val myworld
	| isError ret
		= (ret, ts, ns, myworld)
		= (ret, ts, [NotificationEvent (getDescriptor d) (qw, val):ns], myworld)
	
write` d=:(Parallel sdsX sdsY tfqr tfqw _ tfw) qw val myworld
	= case write` sdsX qwx valx myworld of
		(Error msg, ts, _, _, myworld)     = (Error msg, ts, [], myworld)
		(Ok Void, ts1, ns1, inv1, myworld) = case write` sdsY qwy valy myworld of
										(Error msg, ts, _, _, myworld)     = (Error msg, ts, [], myworld)
										(Ok Void, ts2, ns2, inv2, myworld) = (Ok Void, ts1++ts2, [NotificationEvent (getDescriptor d) (qw, val)):ns1++ns2], myworld)

//:: NotificationEvent = E.qr: NotificationEvent SDSID (qr -> Bool) & TC qr
:: NotificationEvent = E.qw w: NotificationEvent SDSID (qw, w) & TC qw & TC w

notificateAll :: SDSID [NotificationEvent] *MyWorld -> *MyWorld
notificateAll eventsource ns myworld = foldl notificate myworld ns
where
	notificate myworld=:{MyWorld|notification} (NotificationEvent sdsid invalidator)  
			= foldl geninv myworld notification
	where
		// Skip if ...
		geninv myworld {target, origin} | target <> sdsid || (target <> origin && origin == eventsource)
			= myworld
	
		geninv myworld {target, handler, request} | target == sdsid
			= case request of
				(IAlways)			= handler myworld
/*				(IView (qr :: qr^)) = case invalidator qr of
										False = myworld
										True  = handler myworld*/
							 		= myworld	
			
		geninv myworld _ = myworld

// TODO: transaction
upd :: qr (r -> (qw,w,ret)) (Shared qr qw r w *MyWorld) *MyWorld -> *(MaybeErrorString ret, *MyWorld)
upd qr f (Source sds) myworld
	# (ret, myworld) = sds.read qr myworld
	= case ret of
		(Error msg)  	 	= (Error msg, myworld)
		(Ok val)	 		# (qw,w,retval) = f val
					 		# (ret, myworld) = sds.write qw w myworld	// TODO: notification
					 		= (fmap (const retval) ret, myworld)		

class registerForNotification env :: (Shared qr qw r w *env) qr String !*env -> *env | TC qr

instance registerForNotification MyWorld
where
	registerForNotification sds qr msg myworld=:{MyWorld|notification} 
		= {MyWorld | myworld & notification = [createRequest origin (IView (dynamic qr)):lowerLayers++notification]}	
	where
		origin = getDescriptor sds
		createRequest target invreq = 	{ target = target 
										, origin = origin 
										, request = invreq 
										, handler = println ("Notification: "+++msg)
										} 
	
		lowerLayers = [] //createRequest sdsid IAlways \\ sdsid <- tl (collectIds sds)]
	
		collectIds :: (Shared qr qw r w *MyWorld) -> [SDSID]
		collectIds d=:(Source _) = [getDescriptor d]
		collectIds d=:(Projection sds _) = collectIds sds
//		collectIds d=:(ComposedRead sds _) = collectIds sds	
//		collectIds d=:(ComposedWrite sds _) = collectIds sds
//		collectIds d=:(ComposedRW sds _ _) = collectIds sds
		collectIds d=:(Parallel sdsX sdsY _ _ _ _) = collectIds sdsX++collectIds sdsY

//:: T qw w qw` w` = T (qw w -> (qw`, w`))

mapRead :: (qr` -> (qr, r -> r`)) (Shared qr qw r w env) -> (Shared qr` qw r` w env) | TC qr
mapRead tf sds = ComposedRead sds tf

mapWrite :: (qw` -> (qr, r w` -> (qw, w, InvalidateFun qr))) (Shared qr qw r w env) -> (Shared qr qw` r w` env)
mapWrite tf sds = ComposedWrite sds tf

mapReadWrite :: (qr` -> (qr, r -> r`), qw` -> (qr`, r` w` -> (qw, w, InvalidateFun qr`))) (Shared qr qw r w env) -> (Shared qr` qw` r` w` env) | TC qr
mapReadWrite (tfr, tfw) sds = mapWrite tfw (mapRead tfr sds)

//mapReadWrite :: (qr` -> (qr, r -> r`), qw` -> (Maybe qr, (Maybe r) w` -> (qw, w, InvalidateFun qr`))) (Shared qr qw r w env) -> (Shared qr` qw` r` w` env) | TC qr
//mapReadWrite (tfr, tfw) sds = ComposedRW sds tfr tfw

//project :: (qr` -> (qr, r -> r`)) // Read

project :: (Projection qr qw r w qr` qw` r` w`) (Shared qr qw r w env) -> (Shared qr` qw` r` w` env) | TC qr
project pr sds = Projection sds pr

(>+<) infixl 6 :: (Shared qrx qwx rx wx *env) (Shared qry qwy ry wy *env) -> Shared (qrx,qry) (qwx,qwy) (rx,ry) (wx,wy) *env | TC qrx & TC qry
(>+<) sdsX sdsY = Parallel sdsX sdsY id id id id

// -----------------------------------------------------------------------

:: ListFilter = LF_WholeList
			  | LF_Range Int Int
			  | LF_Highest Int  
			// Should be a _filter_ so "LF_Insert Int" doesn't make sense

listView :: (ListFilter -> (Void, [r] -> [r]), 
			 ListFilter -> (Maybe Void, (Maybe [r]) [r] -> (Void, [r], InvalidateFun ListFilter))) | Ord r
			 
listView = (readv, writev)
where
	readv LF_WholeList 		= (Void, id)
	readv (LF_Range s l)	= (Void, \as -> take l (drop s as))
	readv (LF_Highest n)	= (Void, \as -> take n (sortBy (>) as))
		
	writev LF_WholeList		= (Nothing, \Nothing val -> (Void, val, const True))
	writev (LF_Range s l) 	= (Just Void, \(Just as) val -> (Void, setRange as val, invalidator val))
	where	
		setRange as val
			| length as < s
				= as
			| length as < s+l
				= take s as ++ val
				= take s as ++ val ++ drop (s+l) as	

		invalidator _ LF_WholeList = True
		invalidator val (LF_Range s` l`) 
			| length val <> l
				= s` >= s || e` >= s
				= (s` >= s && s` <= e) || (e` >= s && e` <= e) || (s` < s && e` > e) 
		where
			e = s + l
			e` = s` + l` 		

	writev (LF_Highest n) = (Just Void, writev`)
	where
		writev` (Just as) val = (Void, update, invalidator)
		where
			(os, rs) =  splitAt n (sortBy (\a b -> snd a > snd b) (zip2 [1..] as))
			is = take (length (take n val)) (fst (unzip os))
		
			update
				| length as < n
					= val
					# os = zip2 is val
					# as = sortBy (\a b -> fst a < fst b) (os ++ rs)
					= snd (unzip as)

			invalidator _ = True
	
// -----------------------------------------------------------------------

instance fromString Int
where
	fromString str = toInt str

stringView = (stringReadView, stringWriteView)

stringReadView :: q -> (q, r -> String) | toString r
stringReadView q = (q, toString)

stringWriteView :: qw -> (Maybe qr, (Maybe r) String -> (qw, w, InvalidateFun qr)) | fromString w
stringWriteView q = (Nothing, \Nothing val -> (q, fromString val, const True))

// -----------------------------------------------------------------------


//Start :: *World -> (*World, String)
Start world
	# myworld = createMyWorld world
				
	# (memsds1, myworld) = createMemorySDS simpleRW 10 myworld
	# myworld = registerForNotification memsds1 Void "memsds1" myworld
	# (Ok val, myworld) = read memsds1 Void myworld
	# (_, myworld) = write memsds1 Void 11 myworld
	# (_, myworld) = write memsds1 Void 12 myworld	
	# (Ok val, myworld) = read memsds1 Void myworld	

	# (arrsds1, myworld) = createMemorySDS listView [1,2,3] myworld
	# myworld = registerForNotification arrsds1 (LF_Range 0 0) "arrsds1" myworld	
	# (_, myworld) = write arrsds1 (LF_Range 1 1) [] myworld	
	# (_, myworld) = write arrsds1 (LF_Range 0 0) [0] myworld	// This trigers notification
	# (Ok val, myworld) = read arrsds1 (LF_WholeList) myworld

	# compsds = memsds1 >+< arrsds1
	# myworld = registerForNotification compsds (Void,LF_Range 0 0) "compsds" myworld
	# (Ok val, myworld) = write compsds (Void,LF_Range 2 0) (13,[9]) myworld	

//	# (_, myworld) = write memsds1 Void 11 myworld
//	# (Ok val, myworld) = read compsds (Void,LF_WholeList) myworld	

/*
	# (arrsds1, myworld) = createMemorySDS simpleRW 3 myworld
	# arrsds2 = mapRead stringReadView arrsds1
	# arrsds3 = mapWrite stringWriteView arrsds2
	# (_, myworld) = write arrsds3 Void "11" myworld	

	# (arrsds4, myworld) = createMemorySDS simpleRW 4 myworld
	# arrsds5 = mapReadWrite stringView arrsds4
	# (_, myworld) = write arrsds5 Void "6" myworld	

	# (Ok _, myworld)   = write (arrsds3 >+< arrsds5) (Void,Void) ("9","7") myworld
	# (Ok val, myworld) = read (arrsds1 >+< arrsds4) (Void,Void) myworld	
*/

/*
	# (arrsds2, myworld) = createMemorySDS_new listView [5,8,9,2,11] myworld
	# (Ok val, myworld) = read arrsds2 (LF_Highest 3) myworld
	# (_, myworld) = write arrsds2 (LF_Highest 3) [1,4,3] myworld			
	# (Ok val, myworld) = read arrsds2 LF_WholeList myworld
*/

	= (myworld.world, val)


