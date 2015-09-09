definition module resource
 
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


(?:) infix 3 ::  (ResourceClaim r,Resources r) (String,Task a) -> Task a | iTask a & iTask r // task can only be applied when demanded reources are available


showResources 	:: (Resources r) ->  Task [Resource r] | iTask r 
alterResources 	:: (Resources r) ->  Task [Resource r] | iTask r











