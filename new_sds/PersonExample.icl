module PersonExample

import StdEnv, PView, Derived
import Data.Void, Data.Tuple, Data.Error, Data.Func, Data.Either, Text.JSON
from Data.List import splitWith
	
// ONE-TO-MANY RELATION EXAMPLE
:: Person =
    { personId          :: Int
    , name              :: String
    , group             :: String
    , emailAddresses    :: [(Int,String)]
    }

:: PersonRec :== (Int,String,String) //PersonId, Name
:: EmailRec  :== (Int,Int,String) //EmailId, PersonId, EmailAddress

persons :: PView Void [PersonRec] [PersonRec] MyWorld
persons = createStoreView "persons" data
where
    data = [(1,"Alice","family"),(2,"Bob","family"),(3,"Charlie","work"),(4,"Dave","work")]

email :: PView Void [EmailRec] [EmailRec] MyWorld
email = createStoreView "email" data
where
    data = [(1,1,"alice@example.com"),(2,2,"bob@example.com"),(3,2,"bobbie@example.com"),(4,4,"dave@example.net")]

personsByGroup :: PView String [Person] [Person] MyWorld
personsByGroup = pseq personRecsByGroup emailRecsByPersonId paramF writeF readF
where
    paramF recs = [personId \\(personId,_,_) <- recs]
    writeF persons = (map personrecs persons,flatten (map emailrecs persons))
    where
        personrecs {Person|personId,name,group}    = (personId,name,group)
        emailrecs {Person|personId,emailAddresses} = [(emailId,personId,email) \\(emailId,email) <- emailAddresses]

    readF precs erecs = map person precs
    where
        person (personId,name,group) = {Person|personId=personId,name=name,group=group,emailAddresses=email personId}
        email matchId
            = [(emailId,email) \\ (emailId,personId,email) <- erecs | personId == matchId]

personRecsByGroup :: PView String [PersonRec] [PersonRec] MyWorld
personRecsByGroup = applySplit persons (listFilterSplit filterFun) tr1
where
    filterFun match (_,_,group) = group == match

emailRecsByPersonId :: PView [Int] [EmailRec] [EmailRec] MyWorld
emailRecsByPersonId = applySplit email (listFilterSplit filterFun) tr1
where
    filterFun pids (_,personId,_) = isMember personId pids

Start world
	# myworld = createMyWorld world

	# (val, myworld) = get (fixP personsByGroup "family") myworld

	= (val, getWorld myworld)
	
	
	
	
	
	
	
