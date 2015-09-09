implementation module resource
 
import StdArray
import iTasks
 
import iTasks.API.Extensions.Admin.TonicAdmin 

:: Resource a	=	{ kind		:: ResourceKind
					, name		:: a
					, available :: Int
					, inUse		:: [(TaskId,Int)]
					}
:: ResourceKind = 	Consumable | Reusable
:: Requested a	=	{ id		:: a
				  	, demanded	:: Int
				  	}

derive class iTask Resource, ResourceKind, Requested

isAvailable :: [Requested a] [Resource a] -> Bool | iTask a// assumed every resource is mentioned precisely once
isAvailable requests resources
	= and [ available >= demanded 	\\ {name,available} <- resources
									, {id,demanded} <- requests 
									| id === name ]

claimResources :: TaskId [Requested a] [Resource a] -> [Resource a] | iTask a	// assumed that it is tested that they are available
claimResources taskId requests resources 
	= [{name = name, kind = kind, available = available - demanded, inUse = [(taskId,demanded):inUse] }
								\\ {name,kind,available,inUse} <- resources
								, {id,demanded} <- requests 
								| id === name]
	 ++
	 [resource \\ resource <- resources | noMember resource.Resource.name (map (\{id} -> id) requests)]

noMember :: a [a] -> Bool | iTask a
noMember name [] = True
noMember name [name2:rest]
| name === name2 = False
= noMember name rest

refundResources :: TaskId [Requested a] [Resource a] -> [Resource a] |iTask a
refundResources taskId requests resources  
	= [{kind = kind, name = name, available = case kind of 	Reusable 	-> available + demanded
															Consumable	-> available 
								, inUse = [(id,used) \\ (id,used) <- inUse | id <> taskId]
								}
								\\ {name,kind,available,inUse} <- resources
								, {id,demanded} <- requests 
								| id === name
								]
	 ++
	 [resource \\ resource <- resources | noMember resource.Resource.name (map (\{id} -> id) requests)]

findResource :: (Requested a) [Resource a] -> Resource a | iTask a
findResource {id} resources = hd [resource \\ resource <- resources | resource.Resource.name === id]

maxRequested :: [Requested a] [Requested a] -> [Requested a] | iTask a
maxRequested r1 r2 = sortMax (r1 ++ r2) 
where
	sortMax [] 						= []
	sortMax [{id,demanded}:reqs]	= [{id=id,demanded = maxOf demanded same}] ++ sortMax others
	where
		same = [req.demanded \\ req <- reqs | id === req.id]

		maxOf v [] = v
		maxOf v [vv:vvs] = maxOf (if (v>vv) v vv) vvs

		others = [req \\ req <- reqs | id =!= req.id]
		
sumRequested :: [Requested a] [Requested a] -> [Requested a] | iTask a
sumRequested r1 r2 = sumAll (r1 ++ r2) 
where
	sumAll [] 						= []
	sumAll [{id,demanded}:reqs]	= [{id=id,demanded = foldl (+) demanded same}] ++ sumAll others
	where
		same = [req.demanded \\ req <- reqs | id === req.id]

		others = [req \\ req <- reqs | id =!= req.id]

derive gText (,,,,,)

mkDemandTable :: [Requested r] [Resource r] -> Table | iTask r 
mkDemandTable requested res
	 = Table mkHeader tags Nothing
where
	mkHeader  			= ["Demanded","Available?","Resource Name","Kind","Available","In Use"]
	(Table _ tags _)	= toTable 
							(sortBy (\(_,_,id1,_,_,_) (_,_,id2,_,_,_) -> id1<id2)
								[	let resource = findResource req res
									in	( demanded
										, if (isAvailable [req] res) "Yes" "No"
										, toSingleLineText id
										, toSingleLineText resource.kind
										, resource.available 
										, [("(",t,".",i,"):",amount) \\ (TaskId t i,amount) <-  resource.inUse]
										) 
								\\ req=:{id,demanded} <- requested 
								]
							)

mkResourceTable :: [Resource r] -> Table | iTask r 
mkResourceTable  resources
	 = Table mkHeader tags Nothing
where
	mkHeader  			= ["Resource Name","Kind","Available","In Use"]
	(Table _ tags _)	= toTable 
							(sortBy (\(id1,_,_,_) (id2,_,_,_) -> id1<id2)
								[	( toSingleLineText name
									, toSingleLineText kind
									, available 
										, [("(",t,".",i,"):",amount) \\ (TaskId t i,amount) <-  inUse]
										) 
								\\ {name,kind,available,inUse} <- resources 
								]
							)

(?:) infix 3 ::  ([Requested r],Resources r) (String,Task a) -> Task a | iTask a & iTask r 
(?:) (requested,resource) (prompt,task)
 	=						viewSharedInformation prompt
 								[ViewWith (mkDemandTable requested) ] resource
	>>*		[OnAction (Action "OK" /* prompt */[]) 
				(ifValue (\resources -> isAvailable requested resources) 
						 (\_ 		 ->	useResource (requested,resource) task) 
				)
			]

useResource :: ([Requested r],Resources r) (Task a) -> Task a | iTask a & iTask r
useResource (requested,resource) task
	=					withTaskId (return ())
	>>= \(_,taskId) ->	upd (claimResources taskId requested) resource
	>>|					task
	>>= \result ->		upd (refundResources taskId requested) resource
	>>|					return result

showResources :: (Resources r) ->  Task [Resource r] | iTask r 
showResources resources
	=	viewSharedInformation "Available Resources" [ViewWith mkResourceTable] resources

alterResources :: (Resources r) ->  Task [Resource r] | iTask r
alterResources resources
	=	updateSharedInformation "Alter Resources" [] resources



/* alternative with an if ...
(?:) infix 3 ::  [Requested] (String,Task a) -> Task () | iTask a
(?:) requested (prompt,task)
 	=						viewSharedInformation prompt
 								[ViewWith (\res -> [(isAvailable [req] res,req) \\ req <- requested]) ] myResources
	>>= \resources ->		if (isAvailable requested resources) 
									(				upd (claimResources requested) myResources
									>>|				task
									>>= \result ->	upd (refundResources requested) myResources
									>>|				viewInformation "Task Returned:" [] result
									>>|				return ()
									)
									(viewInformation "Cannot perform task due to lack of resources" [] ())

*/
