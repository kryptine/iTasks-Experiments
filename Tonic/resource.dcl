definition module resource
 
import StdArray
import iTasks
 
import iTasks.API.Extensions.Admin.TonicAdmin 

:: Resources r	:== Shared [Resource r]						// administrate your resources in a share
:: Resource r	=	{ kind		:: ResourceKind				// kind of resource							
					, name		:: r						// used to identify a resource, therefore the name should be unique
					, available :: Int						// amount available
					, inUse		:: [(TaskId,Int)]			// list of active tasks which are currently using the resource, and the amount they have claimed
					}
:: ResourceKind = 	Consumable | Reusable
:: Requested r	=	{ id		:: r						
				  	, demanded	:: Int
				  	}

derive class iTask Resource, ResourceKind, Requested

(?:) infix 3 ::  ([Requested r],Resources r) (String,Task a) -> Task a | iTask a & iTask r	// Task can only be launched iff the requested resources are available; the are refunnded after use

isAvailable 	:: [Requested r] [Resource r] -> Bool 		| iTask r												
findResource 	:: (Requested r) [Resource r] -> Resource r | iTask r

// combine requested resources

maxRequested 	:: [Requested r] [Requested r] -> [Requested r] | iTask r				// in case of a choice task
sumRequested 	:: [Requested r] [Requested r] -> [Requested r] | iTask r				// in case of a and task


showResources 	:: (Resources r) ->  Task [Resource r] | iTask r 
alterResources 	:: (Resources r) ->  Task [Resource r] | iTask r






