implementation module resource
 
import StdArray
import iTasks
 
import iTasks.API.Extensions.Admin.TonicAdmin 

:: Resources r		:== Shared [Resource r]
:: Resource r		=	{ kind		:: ResourceKind
						, name		:: r
						, available :: Amount
						, inUse		:: [(TaskId,Amount)]
						}
:: ResourceKind 	= 	Consumable | Reusable
:: Amount			:== Int
:: ResourceClaim r	:== ((Resource r) -> Amount)	

derive class iTask Resource, ResourceKind
derive gText (,,,,,)

// to show the resources in a table

mkResourceTable :: [Resource r] -> Table | iTask r 
mkResourceTable  resources
	 = Table mkHeader tags Nothing
where
	mkHeader  			= ["Resource Name","Kind","Available","In Use"]
	(Table _ tags _)	= toTable 
							(sortBy (\(id1,_,_,_) (id2,_,_,_) -> id1 < id2)
								[	( toSingleLineText name
									, toSingleLineText kind
									, available 
										, [("(",t,".",i,"):",amount) \\ (TaskId t i,amount) <-  inUse]
										) 
								\\ {name,kind,available,inUse} <- resources 
								]
							)

mkDemandTable :: (ResourceClaim r) [Resource r] -> Table | iTask r
mkDemandTable request resources
	 = Table mkHeader tags Nothing
where
	mkHeader  			= ["Demanded","Available?","Resource Name","Kind","Available","In Use"]
	(Table _ tags _)	= toTable 
							(sortBy (\(_,_,id1,_,_,_) (_,_,id2,_,_,_) ->  id1 <  id2)
								[	( request res
									, if (available >= request res) "Yes" "No"
									, toSingleLineText name
									, toSingleLineText kind
									, res.available 
									, [("(",t,".",i,"):",amount) \\ (TaskId t i,amount) <-  inUse]
									) 
								\\ res=:{name,kind,available,inUse} <- resources | request res > 0 
								]
							)

// functions on resources

areAllAvailable :: (ResourceClaim r) [Resource r] -> Bool
areAllAvailable  wantFrom resources 
	= and [ res.available >= wantFrom res
		  \\ res <- resources 
		  | wantFrom res > 0
		  ]

claimResources :: TaskId (ResourceClaim r) [Resource r] -> [Resource r]
claimResources taskId wantFrom resources 
	= 	[   let claim = wantFrom resource in
		{ name		= name
		, kind		= kind
		, available = available - claim
		, inUse 	= if (claim > 0) [(taskId,claim):inUse] inUse
		} 
		\\ resource=:{name,kind,available,inUse} <- resources 
		]

refundResources :: TaskId (ResourceClaim r) [Resource r] -> [Resource r]
refundResources taskId wantFrom resources 
	= 	[   let claim = wantFrom resource in
		{ name		= name
		, kind		= kind
		, available = case kind of 	Reusable 	-> available + claim
									Consumable	-> available 
		, inUse 	= [(id,used) \\ (id,used) <- inUse | id <> taskId]
		} 
		\\ resource=:{name,kind,available,inUse} <- resources 
		]

// tasks

(?:) infix 3 ::  (ResourceClaim r,Resources r) (String,Task a) -> Task a | iTask a & iTask r 
(?:) (requests,resources) (prompt,task)
 	=						viewSharedInformation prompt
 								[ViewWith (mkDemandTable requests) ] resources
	>>*		[OnAction (Action "OK" /* prompt */[]) 
				(ifValue (\resources -> areAllAvailable requests resources) 
						 (\_ 		 ->	useResource requests resources task) 
				)
			]

useResource :: (ResourceClaim r) (Resources r) (Task a) -> Task a | iTask a & iTask r
useResource requests resources task
	=					withTaskId (return ())
	>>= \(_,taskId) ->	upd (claimResources taskId requests) resources
	>>|					task
	>>= \result ->		upd (refundResources taskId requests) resources
	>>|					return result


showResources :: (Resources r) ->  Task [Resource r] | iTask r 
showResources resources
	=	viewSharedInformation "Available Resources" [ViewWith mkResourceTable] resources

alterResources :: (Resources r) ->  Task [Resource r] | iTask r
alterResources resources
	=	updateSharedInformation "Alter Resources" [] resources

