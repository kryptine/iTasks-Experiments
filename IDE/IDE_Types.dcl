definition module IDE_Types

import iTasks
import iTasks.API.Extensions.Development.Codebase
import FindDefinitions
from PmParse import :: IdentifierPositionList

:: IDE_Status =  { codeBase		 :: CodeBase        //Cached shared structured with
                 , openModules   :: [CleanModule]   //Globally tracked opened modules
			     }

//Union of the types of results that the tasks you do in parallel
//in an IDE can have
:: IDE_TaskResult
    = IDE_ModuleEdit IDE_ModuleEdit
    | IDE_SourceTreeEdit FilePath
    | IDE_Search IDE_Search
    | IDE_SettingsEdit
    //|IDE_Build IDE_Build (TODO)

//Relevent observable information of editing a module
:: IDE_ModuleEdit =
    { moduleName    :: ModuleName
    //lastEdit      :: Timestamp (TODO)
    //lastSave      :: Timestamp (TODO)
    }

//Relevent observable information of searching the codebase
:: IDE_Search =
    { query     :: String
    , results   :: [(!CleanFile,!IdentifierPositionList)]
    }

derive class iTask IDE_Status, IDE_TaskResult, IDE_ModuleEdit, IDE_Search

