definition module FindDefinitions


import PmParse 											// old stuf, need to be updated
from UtilStrictLists import :: List, StrictListToList	// old stuf, need to be updated
import iTasks.API.Extensions.Development.Codebase  

:: SearchWhat 	= SearchIdentifier
			  	| SearchImplementation
				| SearchDefinition
				

derive class iTask IdentifierPositionList

/**
* Searches for all occurences or the definition of an identifier in the codebase
* Starts searching in given module
* Recursively searches in all imported modules known in codebase
*
* @param SearchWhat : 		kind of search: all identifiers, c.q. defining occurence in definition or implementation module
* @param Bool:  			search in implementation or definition module
* @param Identifier: 		the identifier to search for
* @param CleanModuleName:   module name to start the search with
* @param CodeBase:   		hierarchical tree with all modules names
* @return					list with all modules where the identifier has been found, with the positions where they occur 		
* 							and list of all modules names which have been searched
*/
searchForIdentifier :: !SearchWhat !Bool !Identifier !(Maybe CleanFile)  !CodeBase 
												-> Task (![(!CleanFile,!IdentifierPositionList)],![CleanFile])

			
