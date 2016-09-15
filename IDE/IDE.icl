module IDE

import iTasks
import iTasks.API.Extensions.Development.Codebase
import iTasks.API.Extensions.CodeMirror, StdFile, StdArray
import qualified Data.Map
import Text, System.OS

import FindDefinitions
//import APIDocumentation

import IDE_Types

//Common actions
ActionSearch :== Action "Search" [ActionIcon "search",ActionKey {key=KEY_ENTER,ctrl=False,shift=False,alt=False}]
ActionAdd    :== Action "Add" []
ActionOpen   :== Action "Open" [ActionTrigger DoubleClick]

//Global status (for all users! If you open a file, everybody opens a file!)
IDE_Status :: (Shared IDE_Status)
IDE_Status = sharedStore  "IDE_Status" 	{ codeBase 	    = []
                                        , openModules   = []
                                        }
initIDE
    =                   updateCodeBase

updateCodeBase
    =                   get IDE_Status
	>>= \status ->		rescanCodeBase status.codeBase
	>>= \codeBase ->	upd (\status -> {status & codeBase = codeBase}) IDE_Status

Start w = startEngine workOnCleanModules w
where
    workOnCleanModules
        =   initIDE
        >>- \initState ->
            parallel [(Embedded,viewLogo) //Yes viewing a useless logo is something we enable you to do :)
                     ,(Embedded,chooseAndAddModules)
                     ,(Embedded,addSearches)
                     :[]/*[(Embedded,closableParTask (\l -> editCleanModule cleanModule l @ IDE_ModuleEdit)) \\ cleanModule <- initState.openModules]*/]
                     []
                     <<@ ArrangeCustom arrange <<@ FullScreen
        @> (updateOpenModules,IDE_Status)

    arrange [b1,b2,b3:bs] actions
        = arrangeWithSideBar 0 LeftSide 200 True
                [ arrangeWithSideBar 0 TopSide 40 False [b1,b2] []
                , arrangeWithSideBar 0 TopSide 40 False [b3,arrangeWithTabs bs []] []] actions

    viewLogo _ = viewInformation () [] (ATag [HrefAttr "http://itasks.org/",TargetAttr "_blank"] [ImgTag [SrcAttr "/img/logo.png",WidthAttr "200",HeightAttr "40"]])
                <<@ ForceLayout <<@ AfterLayout (tweakUI (setMargins 0 0 0 0))
               @? const NoValue

    chooseAndAddModules list
        = 			 	(whileUnchanged (mapRead (\s -> s.codeBase) IDE_Status)
            \codeBase -> ((navigateCodebase codeBase
				        >^* [ OnAction ActionOpen (hasValue (openSelection list))
                            , OnAction ActionAdd (always ((addSourceTree @! ()) <<@ InWindow))
				            ])
				       )) @? const NoValue

    openSelection list (SelSourceTree name rootDir) = openSourceTreeEditor rootDir list
    openSelection list (SelMainModule moduleName moduleBase) = openModuleEditor moduleName MainModule moduleBase list
    openSelection list (SelAuxModule moduleName moduleBase) = openModuleEditor moduleName AuxModule moduleBase list

    addSourceTree :: Task (Maybe SourceTree)
    addSourceTree =
        (   (enterInformation "Please give a descriptive name for this source tree" []
             -&&-
             locateSourceFolder 
             -&&-
             enterInformation "If the source tree is splitted into overlapping parts (for multiplatform subtrees), you can specify the subdirectories here" []
            )
        >>* [OnAction ActionCancel (always (return Nothing))
            ,OnAction ActionAdd (hasValue (\(name,(rootPath,subPaths)) -> add name rootPath subPaths @ Just))
            ]
        ) <<@ Title "Add source tree"
    where
        add name rootPath subPaths =
            let tree =  {SourceTree|name=name,rootPath=rootPath,subPaths=subPaths,modules=[],readOnly=False} in
                    upd (\status -> {status & codeBase = status.codeBase ++ [tree]}) IDE_Status
                >>| updateCodeBase
                @! tree
		
		locateSourceFolder = enterInformation "Please specify the location of the sourcecode on your disk" []

    addSearches list = forever
          (((    (enterInformation () [] <<@ NoAnnotation)
            -&&-
                (updateChoice () [ChooseWith (ChooseFromComboBox view)] [SearchIdentifier,SearchDefinition,SearchImplementation] SearchIdentifier <<@ NoAnnotation)
            )   <<@ ArrangeCustom arrange)
            >>* [OnAction ActionSearch (hasValue (\(query,what) -> openSearch what query list))]
          ) <<@ (Attribute "buttonPosition" "right")
          @? const NoValue
    where
        view SearchIdentifier = "Identifier"
        view SearchDefinition = "Definition"
        view SearchImplementation = "Implementation"

        arrange [block] [] = {UIBlock|block & content = {block.UIBlock.content & direction = Horizontal}} //HACK
        arrange blocks actions = arrangeHorizontal blocks actions

    updateOpenModules (Value results _) status
        | openModules <> status.openModules = Just {status & openModules = openModules}
                                            = Nothing
    where
        openModules = [] //[moduleName \\ (_,Value (IDE_ModuleEdit {IDE_ModuleEdit|moduleName}) _) <- results]

    updateOpenModules _ _ = Nothing

openSourceTreeEditor :: FilePath (SharedTaskList IDE_TaskResult) -> Task ()
openSourceTreeEditor base list
    =   appendTask Embedded (closableParTask (\l -> editSourceTree base l @ IDE_SourceTreeEdit)) list
    >>- \taskId -> focusTask taskId list
    @!  ()

:: SourceTreeOptions = { name :: String, rootPath :: FilePath }
derive class iTask SourceTreeOptions

editSourceTree :: FilePath (SharedTaskList IDE_TaskResult) -> Task FilePath
editSourceTree base list
	//Just edit options for now
	= forever (viewSourceTreeDetails base
			>>* [OnAction ActionEdit (always (editSourceTreeOptions base))]
 	  )
    <<@ Title "Source tree" <<@ Icon "sourcetree"
where
	viewSourceTreeDetails base
		= viewSharedInformation () [ViewWith (fmap view o findSourceTree base)] IDE_Status @! base
	where
		view {SourceTree|name,rootPath} = {SourceTreeOptions|name = name, rootPath = rootPath}

	editSourceTreeOptions base
		= 	get IDE_Status @ findSourceTree base
		>>- \mbSourceTree -> case mbSourceTree of
			Nothing = throw ("Source tree at "+++ base +++ " not found")
			Just {SourceTree|name,rootPath} 
				= 	updateInformation () [] {SourceTreeOptions|name = name, rootPath = rootPath}
				>>= \{SourceTreeOptions|name=newName,rootPath=newRootPath} ->
					upd (\s=:{codeBase} -> {s & codeBase = map (updateTree base newName newRootPath) codeBase}) IDE_Status
				@! base


	updateTree base newName newRootPath tree=:{SourceTree|rootPath}
		| rootPath == base  = {SourceTree|tree & name = newName, rootPath = newRootPath}
							= tree

	findSourceTree base {codeBase} 
		= case [tree \\ tree=:{SourceTree|rootPath} <- codeBase | rootPath == base] of
			[tree] 			= Just tree
			_				= Nothing


openModuleEditor :: ModuleName ModuleType FilePath (SharedTaskList IDE_TaskResult) -> Task ()
openModuleEditor name type base list
    =   appendTask Embedded (closableParTask editorTask) list
    >>- \taskId -> focusTask taskId list
    @!  ()
where
    editorTask = case type of
        MainModule = (\l -> workOnCleanMainModule name base l @ IDE_ModuleEdit)
        AuxModule = (\l -> workOnCleanAuxModule name base l @ IDE_ModuleEdit)

workOnCleanMainModule :: ModuleName FilePath (SharedTaskList IDE_TaskResult) -> Task IDE_ModuleEdit
workOnCleanMainModule moduleName moduleBase list
    = viewInformation () [ViewWith moduleTitleView] moduleName
      ||-
      (editIclFile moduleBase list
       -&&-
       buildMainModule moduleName moduleBase
        <<@ ArrangeWithTabs
      ) <<@ ArrangeWithSideBar 0 TopSide 35 False <<@ (Title moduleName)  <<@ Icon "mainmodule"
    @! {IDE_ModuleEdit|moduleName=moduleName}

workOnCleanAuxModule :: ModuleName FilePath (SharedTaskList IDE_TaskResult) -> Task IDE_ModuleEdit
workOnCleanAuxModule moduleName moduleBase list
    = viewInformation () [ViewWith moduleTitleView] moduleName
      ||-
      (editDclFile moduleBase list
       -&&-
       editIclFile moduleBase list
        <<@ ArrangeWithTabs
      ) <<@ ArrangeWithSideBar 0 TopSide 35 False <<@ (Title moduleName) <<@ Icon "auxmodule"
    @! {IDE_ModuleEdit|moduleName=moduleName}

editIclFile :: FilePath  (SharedTaskList IDE_TaskResult) -> Task ()
editIclFile moduleBase list
    = editCleanFile (addExtension moduleBase "icl") list <<@ Title "Implementation"

editDclFile :: FilePath (SharedTaskList IDE_TaskResult) -> Task ()
editDclFile moduleBase list
    = editCleanFile (addExtension moduleBase "dcl") list <<@ Title "Definition"

editCleanFile :: FilePath (SharedTaskList IDE_TaskResult) -> Task ()
editCleanFile path list = catchAll
   (    importTextFile path @ split OS_NEWLINE
   >>- \initContent ->
        withShared (initCleanEditor False initContent)
            \mirror -> updateCleanEditor (shareSearchResults path list mirror) @! ()
   ) (\e -> viewInformation () [] e @! ())

moduleTitleView s = SpanTag [StyleAttr "font-size: 24px"] [Text s]

buildMainModule :: FilePath ModuleName -> Task IDE_ModuleEdit
buildMainModule base moduleName
    = viewInformation () [] /*filterSearchers*/ ("Building is not supported in this version") <<@ Title "Build"
    @? const NoValue

shareSearchResults :: FilePath (SharedTaskList IDE_TaskResult) (Shared CodeMirror)  -> (Shared CodeMirror)
shareSearchResults path list mirror  = mirror
//= 		list >+> filterSearchers //TODO Use proper share propagation to link shares
/*
= sdsSequence ">+>"  (\p r -> p) filterSearchers 
						(SDSWriteConst (\p w -> Ok (Just w))) 
							(SDSWriteConst (\p w -> Ok Nothing)) 
								mirror list //filterSearchers list //TODO Use proper share propagation to link shares
*/
where
    filterSearchers :: (CodeMirror, TaskList IDE_TaskResult) -> CodeMirror
    filterSearchers (mirror, (_,items))
    # highLight 		=  [ toHighLight (size query) (toList posList) 
    										\\ {TaskListItem|value=Value (IDE_Search {results,query}) _} <- items
                                            ,  ((sBase,sModule,sExt),posList) <- results
                                            |  addExtension (sBase </> sModule) (toString sExt) == path
                           ]
    = case highLight of
        [] 		-> mirror
        lights  -> {mirror & highlighted = flatten lights} 

	toHighLight length list = map (\(line,col) -> ((line,col),(line,col+length-1))) list

undef = undef
//sdsSequence :: !String !(p r1 -> p2) !((r1,r2) -> r) !(SDSLensWrite p w r1 w1) !(SDSLensWrite p w r2 w2) !(RWShared p r1 w1) !(RWShared p2 r2 w2) -> RWShared p r w | TC p2


openSearch :: SearchWhat String (SharedTaskList IDE_TaskResult) -> Task ()
openSearch what identifier list
    =   appendTask Embedded (closableParTask (\l -> searchCodebase what identifier l @ IDE_Search)) list
    >>- \taskId -> focusTask taskId list
    @!  ()

searchCodebase what initq list
    = withShared initq
    \query -> (
        (updateSharedInformation () [] query)
        ||-
        (whileUnchanged query
            \identifier ->
                get IDE_Status
            >>- \status ->
                searchForIdentifier what True identifier Nothing status.codeBase
            >>- \results ->
                viewSearchResults what identifier (fst results) list
            @! {IDE_Search|query=identifier,results=fst results}
        )) <<@ (ArrangeWithSideBar 0 TopSide 50 False) <<@ (Title "Search") <<@ Icon "search"
where
    viewSearchResults :: !SearchWhat !String [(!CleanFile,!IdentifierPositionList)] (SharedTaskList IDE_TaskResult) -> Task (!CleanFile,!IdentifierPositionList)
    viewSearchResults what identifier [] list
        =	viewInformation (Title "Results") [] (toString what identifier +++ "has *not* been found !") @? const NoValue
    viewSearchResults what identifier found list
        =   enterChoice ("Results",toString what identifier +++ "has been found in:") [ChooseWith (ChooseFromGrid toGrid)] found
        >^* [OnAction (Action "Open module" [ActionTrigger DoubleClick]) (hasValue (\((dir,module,_),_) -> openSearchResult module list))]

    openSearchResult module list
        =   (get IDE_Status @ \status -> lookupModule module status.codeBase)
        >>- maybe (throw ("Could not find module "+++ module))
                  (\(modName,modType,modPath) -> openModuleEditor modName modType modPath list)

	toString SearchIdentifier 	  ident  = "Identifier \""        +++ ident +++ "\" "
	toString SearchImplementation ident  = "Implementation of \"" +++ ident +++ "\" "
	toString SearchDefinition     ident  = "Definition of \""     +++ ident +++ "\" "

    toGrid :: (!CleanFile,!IdentifierPositionList) -> FoundInfo
    toGrid ((pathName,modName,ext),positions) = { moduleName = modName
    											, iclDcl	 = ext
    											, howOften 	 = length (toList positions) 
    											, positions	 = toList positions
    											, pathName 	 = pathName
    											}



:: FoundInfo =  { moduleName 	:: ModuleName
				, iclDcl		:: Extension
				, howOften		:: Int
				, positions		:: [(Int,Int)]
				, pathName		:: FilePath
				}

derive class iTask FoundInfo, SearchWhat

(>>?) infixl 1 :: !(Task a) !(a -> Task b) -> Task (Maybe b) | iTask a & iTask b
(>>?) taska taskbf = step taska (const Nothing)
    [OnAction ActionCancel          (always (return Nothing))
    ,OnAction ActionOk              (hasValue (\a -> taskbf a @ Just))
    ,OnValue                        (ifStable (\a -> taskbf a @ Just))
    ]

getSelection :: CodeMirror -> Identifier
getSelection {position,selection=Nothing,source} =  "nothing"
getSelection {position,selection=Just ((beginline,beginchar),(endline,endchar)),source}
    | beginline == endline && beginchar == endchar =  "zero"
    = join OS_NEWLINE (take (endline - beginline) (drop beginline source)) //TODO, ALSO SELECT CHARS

closableParTask :: (ParallelTask a) -> (ParallelTask a) | iTask a
closableParTask task = task`
where
    task` l =   get (taskListSelfId l)
            >>- \myId ->
                (task l -|| (viewInformation () [] () >>* [OnAction ActionClose (always (removeTask myId l))]))

toList :: IdentifierPositionList -> [(Int,Int)]
toList (Pos begin end rest) = [(begin,end): toList rest]
toList _ = []

NoAnnotation	:== AfterLayout (tweakControls (map (\(c,_) -> (c,'Data.Map'.newMap))))

