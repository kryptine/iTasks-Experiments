module TaskExample

import StdEnv, PView, Derived, Text.JSON
from Data.List import splitWith

:: TaskInstance =
    { instanceId    :: Int
    , instanceType  :: TaskInstanceType
    , instanceTags  :: [String]          //For selecting stuff
    , instanceState :: String            //Normally this is what you update when you process task events
    }

:: TaskInstanceType = SessionTask | PersistentTask

:: TaskInstanceFilter =
    { filterById    :: Maybe Int
    , filterByType  :: Maybe TaskInstanceType
    , filterByTag   :: Maybe String
    }

emptyFilter :: TaskInstanceFilter
emptyFilter = {filterById = Nothing,filterByType=Nothing,filterByTag=Nothing}

instance == TaskInstanceType
where
	(==) SessionTask SessionTask = True
	(==) PersistentTask PersistentTask = True
	(==) _ _ = False

derive JSONEncode TaskInstance, TaskInstanceType
derive JSONDecode TaskInstance, TaskInstanceType

instanceTableData :: [TaskInstance]
instanceTableData =
			[{instanceId=1,instanceType=PersistentTask,instanceTags = ["old"],instanceState = "Hansje"}
            ,{instanceId=2,instanceType=SessionTask,instanceTags = [],instanceState = "Pansje"}
            ,{instanceId=3,instanceType=SessionTask,instanceTags = [],instanceState = "Kevertje"}
            ,{instanceId=4,instanceType=PersistentTask,instanceTags = ["work","personal"],instanceState = "Die"}
            ,{instanceId=5,instanceType=SessionTask,instanceTags = [],instanceState = "Zat"}
            ,{instanceId=6,instanceType=PersistentTask,instanceTags = ["important","work"],instanceState = "Eens"}
            ,{instanceId=7,instanceType=PersistentTask,instanceTags = ["work"],instanceState = "Op"}
            ,{instanceId=8,instanceType=SessionTask,instanceTags = [],instanceState = "Een"}
            ,{instanceId=9,instanceType=SessionTask,instanceTags = [],instanceState = "Hek"}
            ,{instanceId=10,instanceType=PersistentTask,instanceTags = [],instanceState = "Toen"}
            ,{instanceId=11,instanceType=PersistentTask,instanceTags = ["important"],instanceState = "Kwam"}
            ,{instanceId=12,instanceType=SessionTask,instanceTags = [],instanceState = "De"}
            ,{instanceId=13,instanceType=SessionTask,instanceTags = [],instanceState = "Regen"}
            ]

instanceTable :: PView Void [TaskInstance] [TaskInstance] MyWorld
instanceTable = createStoreView "instanceTable" instanceTableData

filteredInstances :: PView TaskInstanceFilter [TaskInstance] [TaskInstance] MyWorld
filteredInstances = applySplit instanceTable {sget = sget`, sput = sput`} tr1
where
    sget` tfilter is = filter (filterFun tfilter) is
    sput` tfilter is ws
        = let (ds,us) = splitWith (filterFun tfilter) is
          in (us ++ ws, notifyFun (ds ++ ws))

    filterFun {filterById,filterByType,filterByTag}
        = \i -> (maybe True (\m -> i.instanceId == m) filterById)
            &&  (maybe True (\m -> i.instanceType == m) filterByType)
            &&  (maybe True (\m -> isMember m i.instanceTags) filterByTag)

    notifyFun ws qfilter = any (filterFun qfilter) ws

instancesOfType :: PView (TaskInstanceType,TaskInstanceFilter) [TaskInstance] [TaskInstance] MyWorld
instancesOfType = applyTranslation filteredInstances (\(t,f) -> {f & filterByType = Just t})

persistentInstances :: PView TaskInstanceFilter [TaskInstance] [TaskInstance] MyWorld
persistentInstances = applyTranslation instancesOfType (\f -> (PersistentTask,f))

sessionInstances :: PView TaskInstanceFilter [TaskInstance] [TaskInstance] MyWorld
sessionInstances = applyTranslation instancesOfType (\f -> (SessionTask,f))

persistentWithTag :: PView (String,TaskInstanceFilter) [TaskInstance] [TaskInstance] MyWorld
persistentWithTag = applyTranslation persistentInstances (\(t,f) -> {f & filterByTag = Just t})

importantInstances :: PView TaskInstanceFilter [TaskInstance] [TaskInstance] MyWorld
importantInstances = applyTranslation persistentWithTag (\f -> ("important",f))

instanceById :: PView Int TaskInstance TaskInstance MyWorld
instanceById = applyLens (applyTranslation filteredInstances (\i -> {emptyFilter & filterById = Just i})) singletonLens

instanceByTag :: PView String [TaskInstance] [TaskInstance] MyWorld
instanceByTag = applyTranslation filteredInstances (\t -> {emptyFilter & filterByTag = Just t})

instancesOfTypeSplit = {sget = sget`, sput = sput`}
where
	sget` type is = filter (\{instanceType} -> instanceType == type) is
	sput` type is ws = (filter (\{instanceType} -> instanceType <> type) is ++ ws, (==) type)

tagSplit = {sget = sget`, sput = sput`}
where
	sget` tag is = [i \\ i <- is | isMember tag i.instanceTags]
	sput` tag is ws = (kept ++ ws, \tag = isMember tag affectedtags)
	where
		kept = [i \\ i <- is | not (isMember tag i.instanceTags)]
		replaced = [i \\ i <- is | isMember tag i.instanceTags]
		affectedtags = removeDup (flatten (map (\{instanceTags} -> instanceTags) (replaced ++ ws)))

instanceIdSplit = {sget = sget`, sput = sput`}
where
	sget` iid is = filter (\{instanceId} -> instanceId == iid) is
	sput` iid is ws = (filter (\{instanceId} -> instanceId <> iid) is ++ ws, (==) iid)

idtagifun _ os is tag = any (\{instanceTags} -> isMember tag instanceTags) (is++os)
tagidifun _ os is id = any (\{instanceId} -> id == instanceId) (is++os)

Start world
	# myworld = createMyWorld world

	# myworld = registerForNotification instanceByTag "new" "Tag 'NEW'" myworld	
	# myworld = registerForNotification filteredInstances {emptyFilter & filterByTag = Just "new", filterByType = Just PersistentTask} "Tag 'new'" myworld
	# myworld = registerForNotification filteredInstances {emptyFilter & filterByTag = Just "old", filterByType = Just PersistentTask} "Tag 'old' P" myworld

	# (_, myworld) = put (fixP filteredInstances {emptyFilter & filterByTag = Just "old", filterByType = Nothing}) [{instanceId=4, instanceType=SessionTask, instanceTags = ["new"],instanceState = "Hansje2"}] myworld

	# (val, myworld) = get instanceTable myworld
	# (val, myworld) = get (fixP filteredInstances {emptyFilter & filterByTag = Just "old", filterByType = Just PersistentTask}) myworld

	= (val, getWorld myworld)
	
	
	
	
	
	
	
