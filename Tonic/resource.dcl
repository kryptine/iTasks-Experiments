definition module resource
 
import StdArray
import iTasks
 
import iTasks.API.Extensions.Admin.TonicAdmin 

:: Resource r		=	{ kind		:: ResourceKind				// 
						, name		:: r						// r can be of any type | iTask r
						, available :: Amount					// choose a sensible dimension here, e.g. percentage, liter, euro.s, ...
						, inUse		:: [(TaskId,Amount)]		// which task is occupying a certain amount
						}
:: ResourceKind 	= 	Consumable | Reusable					// reusable resources return the amount they occupy after use
:: Amount			:== Int
:: ResourceClaim r	:== ( (Resource r) -> Amount				// select the resources you want to use, 0 means: resource is not needed
						, [Resource r] -> [Resource r]			// tell which is of the selected resources you really need, id means: all needed
						)

derive class iTask Resource, ResourceKind

(?:) infix 3 ::  (ResourceClaim r,Shared [Resource r]) (String,Task a) -> Task a | iTask a & iTask r 

showResources 	:: (Shared [Resource r]) ->  Task [Resource r] | iTask r 
alterResources 	:: (Shared [Resource r]) ->  Task [Resource r] | iTask r











