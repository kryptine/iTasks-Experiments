implementation module FindDefinitions

// Searches for definitions in Clean modules

import PmParse 
from 	UtilStrictLists import :: List
import	iTasks.API.Extensions.Development.Codebase
import System.FilePath  

derive class iTask IdentifierPositionList

// Clean system syntax dependant parsing

from UtilStrictLists import :: List, StrictListToList, ListToStrictList

searchForIdentifier :: !SearchWhat !Bool !Identifier !(Maybe CleanFile)  !CodeBase 
												-> Task (![(!CleanFile,!IdentifierPositionList)],![CleanFile])
searchForIdentifier what inImports identifier maybeName codeBase
    # allNames = case what of
    				SearchDefinition 	 -> [fileNames \\ fileNames =: (filePath,moduleName,Dcl) <- listFilesInCodeBase codeBase ]			
    				SearchImplementation -> [fileNames \\ fileNames =: (filePath,moduleName,Icl) <- listFilesInCodeBase codeBase ]
    				_ -> 					listFilesInCodeBase codeBase 		
    = maybe (if (allNames == []) (return ([],[])) (search allNames [] [] []))
            (\name -> search [name] allNames [] []) maybeName
where
	search ::  ![CleanFile] ![CleanFile] ![CleanFile] [(!CleanFile,!IdentifierPositionList)] 
												-> Task (![(!CleanFile,!IdentifierPositionList)],![CleanFile])
	search [] notSearched searched found			
		=   return (reverse found,reverse searched)								
	search [file:rest] notSearched searched found
		=   searchInFile what False identifier (cleanFilePath file)
		>>- continue
	where
		continue (imported,pos) // modules imported by searched file, found identifiers
		# nfound 			= ifPosNil pos found [(file,pos):found]
		# (toDo,restBase)   = searchModulesInCodeBase imported notSearched
		# toSearch		    = removeDup (rest ++ toDo)
		= search toSearch restBase [file:searched] nfound	
	
	ifPosNil PosNil then else = then
	ifPosNil _      then else = else

// returns (module names imported, positions where identifier has been found)
searchInFile :: !SearchWhat !Bool !Identifier !String -> Task (![String],!IdentifierPositionList)
searchInFile SearchIdentifier inImports identifier fileName 
	= 					accWorld (accFiles (FindIdentifiersInFile inImports [!!] identifier fileName ))
	>>= \(list,pos) ->  return (StrictListToList list,pos)
searchInFile SearchImplementation inImports identifier fileName 
	= 					accWorld (accFiles (FindDefinitionInFile inImports [!!] identifier fileName ))
	>>= \(list,pos) ->  return (StrictListToList list,pos)
searchInFile SearchDefinition inImports identifier fileName 
	= 					accWorld (accFiles (FindDefinitionInFile inImports [!!] identifier fileName ))
	>>= \(list,pos) ->  return (StrictListToList list,pos)
	
searchModulesInCodeBase :: ![ModuleName] ![CleanFile] -> ([CleanFile],[CleanFile])
searchModulesInCodeBase moduleNames codeBase
	= span (\(base,name,ext) -> isMember name moduleNames) codeBase

