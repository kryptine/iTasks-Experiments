implementation module resource
 
import StdArray
import iTasks
 
import iTasks.API.Extensions.Admin.TonicAdmin 

:: Resource r		=	{ kind		:: ResourceKind
						, name		:: r
						, available :: Amount
						, inUse		:: [(TaskId,Amount)]
						}
:: ResourceKind 	= 	Consumable | Reusable
:: Amount			:== Int
:: ResourceClaim r	:== ((Resource r) -> Amount, [Resource r] -> [Resource r])

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
mkDemandTable (claim, check) resources
	 = Table mkHeader tags Nothing
where
	mkHeader  			= ["Demanded","Available?","Resource Name","Kind","Available","In Use"]
	(Table _ tags _)	= toTable 
							(sortBy (\(_,_,id1,_,_,_) (_,_,id2,_,_,_) ->  id1 <  id2)
								[	( claim res
									, if (available >= claim res) "Yes" "No"
									, toSingleLineText name
									, toSingleLineText kind
									, res.available 
									, [("(",t,".",i,"):",amount) \\ (TaskId t i,amount) <-  inUse]
									) 
								\\ res=:{name,kind,available,inUse} <- selectResources (claim, check) resources
								]
							)

// functions on resources

areAllAvailable :: (ResourceClaim r) [Resource r] -> Bool
areAllAvailable (claim, filter) resources 
	= case [res.available >= claim res \\ res <- selectResources (claim, filter) resources] of
		[] -> False
		else -> and else

selectResources :: (ResourceClaim r) [Resource r] -> [Resource r]
selectResources (claim, filter) resources 
	= filter [res \\ res <- resources | claim res > 0]

reserveAll :: TaskId (ResourceClaim r) [Resource r] -> [Resource r] | iTask r
reserveAll taskId (claim, filter) resources 
	= 	[ let amount = claim resource in
		{ name		= name
		, kind		= kind
		, available = available - amount
		, inUse 	= if (amount > 0) [(taskId,amount):inUse] inUse
		} 
		\\ resource=:{name,kind,available,inUse} <- selected
		] 
		++
		removeDups selected resources
where
	selected = selectResources (claim, filter) resources

	removeDups [] resources 	= resources
	removeDups [r:rs] resources = removeDups rs [res \\ res <- resources | r.Resource.name =!= res.Resource.name]	

refundAll :: TaskId [Resource r] -> [Resource r]
refundAll taskId resources 
	= 	[   
		{ name		= name
		, kind		= kind
		, available = case kind of 	Reusable 	-> available + case [amount \\ (id,amount) <- inUse | id == taskId] of
																[amount:_] -> amount
																_		   -> 0
									Consumable	-> available 
		, inUse 	= [(id,used) \\ (id,used) <- inUse | id <> taskId]
		} 
		\\ resource=:{name,kind,available,inUse} <- resources
		]

// tasks

(?:) infix 3 ::  (ResourceClaim r,Shared [Resource r]) (String,Task a) -> Task a | iTask a & iTask r 
(?:) (requests,resources) (prompt,task)
 	=						viewSharedInformation prompt
 								[ViewWith (mkDemandTable requests) ] resources
	>>*		[OnAction (Action "OK" /* prompt */[]) 
				(ifValue (\resources -> areAllAvailable requests resources) 
						 (\_ 		 ->	useResource requests resources task) 
				)
			]

useResource :: (ResourceClaim r) (Shared [Resource r]) (Task a) -> Task a | iTask a & iTask r
useResource requests resources task
	=					withTaskId (return ())
	>>= \(_,taskId) ->	upd (reserveAll taskId requests) resources
	>>|					task
	>>= \result ->		upd (refundAll taskId) resources
	>>|					return result


showResources :: (Shared [Resource r]) ->  Task [Resource r] | iTask r 
showResources resources
	=	viewSharedInformation "Available Resources" [ViewWith mkResourceTable] resources

alterResources :: (Shared [Resource r]) ->  Task [Resource r] | iTask r
alterResources resources
	=	updateSharedInformation "Alter Resources" [] resources

