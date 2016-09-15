module JSPlumbdemo

import iTasks, Tasklet
import Text.StringAppender, graph_to_sapl_string
import sapldebug

Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist taskletExamples)) world

taskletExamples :: [Workflow]
taskletExamples =
	[workflow "JSPlumb tasklet" "Simple JSPlumb tasklet" plumbtasklet]

plumbtasklet :: Task Void
plumbtasklet
	= 		mkInstanceId >>= \iid ->
	 		mkTask (iid, jsPlumbTasklet)
							
targetOptions = {mkEndpoint & anchor 			= Just [toHtmlObject "TopCenter"]
							, maxConnections 	= Just -1
							, isTarget 			= Just True
							, endpoint 			= Just [toHtmlObject "Dot",toHtmlObject {radius = 5}]
							, paintStyle 		= Just {fillStyle =  "#66CC00"}
                }
                
sourceOptions = {mkEndpoint & anchor 			= Just [toHtmlObject "BottomCenter"]
							, maxConnections 	= Just -1
							, isSource 			= Just True
							, endpoint 			= Just [toHtmlObject "Dot",toHtmlObject {radius = 5}]
							, paintStyle 		= Just {fillStyle =  "#EEDD00"}
                }

:: EndPointOptions =
	{ anchor    				:: Maybe [HtmlObject]
	, endpoint 					:: Maybe [HtmlObject]
	, enabled 					:: Maybe Bool
    , paintStyle      		 	:: Maybe FillStyle
    , hoverPaintStyle 			:: Maybe FillStyle
    , cssClass 					:: Maybe String
    , hoverClas 				:: Maybe String
    , source 					:: Maybe String
    , canvas 					:: Maybe HtmlObject
    , container 				:: Maybe String
    , connections 				:: Maybe [HtmlObject]
	, isSource  				:: Maybe Bool
	, maxConnections  			:: Maybe Int
	, dragOptions 				:: Maybe HtmlObject
	, connectorStyle 			:: Maybe FillStyle
	, connectorHoverStyle 		:: Maybe FillStyle
	, connector 				:: Maybe [HtmlObject]
	, connectorOverlays 		:: Maybe [HtmlObject]
	, connectorClass 			:: Maybe String
	, connectorHoverClass 		:: Maybe String
	, connectionDetachable 		:: Maybe Bool
    , isTarget  				:: Maybe Bool
    , dropOptions 				:: Maybe HtmlObject
    , reattach 					:: Maybe Bool
    , parameters 				:: Maybe HtmlObject
    }

mkEndpoint :: EndPointOptions
mkEndpoint =
	{ anchor    				= Nothing
	, endpoint 					= Nothing
	, enabled 					= Nothing
    , paintStyle      		 	= Nothing
    , hoverPaintStyle 			= Nothing
    , cssClass 					= Nothing
    , hoverClas 				= Nothing
    , source 					= Nothing
    , canvas 					= Nothing
    , container 				= Nothing
    , connections 				= Nothing
	, isSource  				= Nothing
	, maxConnections  			= Nothing
	, dragOptions 				= Nothing
	, connectorStyle 			= Nothing
	, connectorHoverStyle 		= Nothing
	, connector 				= Nothing
	, connectorOverlays 		= Nothing
	, connectorClass 			= Nothing
	, connectorHoverClass 		= Nothing
	, connectionDetachable 		= Nothing
    , isTarget  				= Nothing
    , dropOptions 				= Nothing
    , reattach 					= Nothing
    , parameters 				= Nothing
    }

:: Radius = {radius :: Int}
:: FillStyle = {fillStyle :: String}

:: PlumbState = {plumb :: Maybe HtmlObject}

jsPlumbTasklet ::  Tasklet PlumbState Void
jsPlumbTasklet = 
	{ generatorFunc		= jsPlumbGUI
	, resultFunc		= \_ -> Value Void False
	, tweakUI  			= setTitle "JSPlumb Tasklet"
	}
where
	jsPlumbGUI iid taskId Nothing iworld 
		= jsPlumbGUI iid taskId (Just {plumb = Nothing}) iworld

	jsPlumbGUI iid _ (Just st) iworld

		# canvas = DivTag [IdAttr "plumb_canvas", StyleAttr "width:100%; height:100%"] []

		# gui = { TaskletHTML
				| width  		= ExactSize 600
				, height 		= ExactSize 600
				, html   		= HtmlDef (html canvas)
				, eventHandlers = [HtmlEvent "tasklet" "init" onInit
				                  ,HtmlEvent "tasklet" "destroy" onDestroy
				                  ,HtmlEvent "tasklet" "afterlayout" onAfterLayout]
				}
			
		= (TaskletHTML gui, st, iworld)
				
	where

	    onScriptLoad st _ _ d
		    # (d, _) = setDomAttr d "plumb_canvas" "innerHTML"
		    				("<div class='node' id='block1' style='position: absolute; top: 50px; left: 50px; border: 1px solid black;'>Block 1</div>" +++
		    				 "<div class='node' id='block2' style='position: absolute; top: 100px; left: 100px; border: 1px solid black;'>Block 2</div>")
		    
		    # (d, plumb)    = jsPlumb d
			# (d, plumb, _) = runObjectMethod d plumb "ready" [createEventHandler onReady iid]
			= (d, {st & plumb = Just plumb})

		onInit st iid e d
			# (d, plumb) = jsPlumb d
			| isUndefined plumb 
			= (loadPlumbAPI iid e d, st)
			= onScriptLoad st iid e d
		
		onReady st iid e d
			# (d, plumb) = jsPlumb d
			# (d, plumb) = addEndpoint plumb "block1" targetOptions d
			# (d, plumb) = addEndpoint plumb "block1" sourceOptions d
			# (d, plumb) = addEndpoint plumb "block2" targetOptions d
			# (d, plumb) = addEndpoint plumb "block2" sourceOptions d
			# (d, plumb) = draggable plumb "block1" d
			# (d, plumb) = draggable plumb "block2" d
			= (d, plumb)
		
		loadPlumbAPI _ _ d = d

		nullEventHandler st _ _ d = (d, st)

		onDestroy st=:{plumb = Just plumb} _ _ d
			// clear generated stuff
			# (d, _) = setDomAttr d "plumb_canvas" "innerHTML" ""
		
			= (d, {st & plumb = Nothing})

		onDestroy st _ _ d
			= (d, st)

		onAfterLayout st _ _ d
			= (d, st)

jsPlumb :: *HtmlDocument -> *(*HtmlDocument,HtmlObject)
jsPlumb d = findObject d "jsPlumb"

bind :: HtmlObject String HtmlObject *HtmlDocument -> *(*HtmlDocument, HtmlObject)
bind plumb event cb d
	# (d, p, _) = runObjectMethod d plumb "bind" [toHtmlObject event, toHtmlObject cb]
	= (d, p)

addEndpoint :: HtmlObject String EndPointOptions *HtmlDocument -> *(*HtmlDocument,HtmlObject)
addEndpoint plumb target opts d
	# (d, p, _) = runObjectMethod d plumb "addEndpoint" [toHtmlObject target, toHtmlObject opts]
	= (d, p)

draggable :: HtmlObject String *HtmlDocument -> *(*HtmlDocument,HtmlObject)
draggable plumb target d
	# (d, p, _) = runObjectMethod d plumb "draggable" [toHtmlObject target]
	= (d, p)

//-------------------------------------------------------------------------

ifValue pred (Value v _) | pred v
	= Just (return v)
	= Nothing

ifStable (Value v True) = Just (return v)
ifStable _				= Nothing

returnC :: b (TaskValue a) -> Task b | iTask b
returnC v _ = return v

returnV :: (TaskValue a) -> Task a | iTask a
returnV (Value v _) = return v
    
