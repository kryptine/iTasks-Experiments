module TaskletExamples

import iTasks, iTasks.API.Core.Client.Tasklet, iTasks.API.Core.Client.Interface

//-------------------------------------------------------------------------
// http://www.sephidev.net/external/webkit/LayoutTests/fast/dom/Geolocation/argument-types-expected.txt
// http://html5doctor.com/finding-your-position-with-geolocation/

:: GeoLocationParams = {enableHighAccuracy :: Bool
					   ,timeout            :: Int
					   ,maximumAge         :: Int
					   }

:: GPSCoord :== (Real,Real)

geoTasklet :: Tasklet (Maybe GPSCoord) (Maybe GPSCoord)
geoTasklet = 
	{ genUI				= geoTaskletGUI
	, resultFunc		= \pos = Value pos False
	, tweakUI  			= setTitle "GEO Tasklet"
	}

geoTaskletGUI _ _ iworld

	# gui = { width  		= ExactSize 300
			, height 		= ExactSize 30
			, html   		= RawText "Current position: <span id='loc'/>"
			, eventHandlers = [ComponentEvent "tasklet" "init" onInit]
			}
			
	= (TaskletHTML gui, Nothing, iworld)
where
    onSuccess _ pos st world
		# (la, world) = jsGetObjectAttr "coords.latitude" pos world
		# (lo, world) = jsGetObjectAttr "coords.longitude" pos world
		# world = setDomAttr "loc" "innerHTML" (toJSVal (jsValToString la +++ ", " +++ jsValToString lo)) world
    	= (Just (la,lo), world)

    onFailure _ msg st world
		# world = setDomAttr "loc" "innerHTML" (toJSVal "FAILURE") world
    	= (st, world)

	onInit taskId _ st world
	    # (loc, world) = findObject "navigator.geolocation" world
		# (_, world)   = callObjectMethod "getCurrentPosition" 
								[toJSArg (createTaskletEventHandler onSuccess taskId)
								,toJSArg (createTaskletEventHandler onFailure taskId)
							    ,toJSArg {enableHighAccuracy = True, timeout = 10 * 1000 * 1000, maximumAge = 0}]
					    		loc world
				
		= (st, world)

//-------------------------------------------------------------------------

pushTasklet :: Tasklet Int Int 
pushTasklet = 
	{ genUI				= pushGenerateGUI
	, resultFunc		= \i = Value i False
	, tweakUI  			= setTitle "Push Tasklet"
	}

pushGenerateGUI :: !TaskId (Maybe Int) !*IWorld -> *(!TaskletGUI Int, !Int, !*IWorld)

pushGenerateGUI taskId Nothing iworld  = pushGenerateGUI taskId (Just 1) iworld

pushGenerateGUI _ (Just st) iworld  

	# gui = { width  		= ExactSize 50
			, height 		= ExactSize 27
			, html   		= RawText ("<input type=\"button\" id=\"pushbtn\" name=\"push\" value=\""+++ toString st +++"\">")
			, eventHandlers = [ComponentEvent "pushbtn" "click" onClick]
			}
			
	= (TaskletHTML gui, st, iworld)
where			
	onClick _ _  state world
		# world = setDomAttr "pushbtn" "value" (toJSVal (toString (state + 1))) world
		= (state + 1, world)
 
//-------------------------------------------------------------------------

:: PainterState = 	{ tool 		:: String
					, color 	:: String
					, mouseDown :: Maybe (Int, Int)
					, lastDraw  :: Maybe DrawType
					, draw		:: ![DrawType]
					, finished  :: Bool				
					}

:: DrawType = DrawLine !String !Int !Int !Int !Int
		    | DrawRect !String !Bool !Int !Int !Int !Int
		    | DrawCircle !String !Bool !Int !Int !Int !Int

:: Drawing = Drawing [DrawType]

derive class iTask PainterState, DrawType, Drawing

info = "Draw something, but use the pencil _slowly_ in Chrome!" 

painterTasklet :: Tasklet PainterState Drawing
painterTasklet = 
	{ genUI				= painterGenerateGUI
	, resultFunc		= \{draw,finished} = Value (Drawing draw) finished
	, tweakUI  			= setTitle "Drawing Tasklet"
	}

canvasWidth :== 300
canvasHeight :== 300

// TODO: http://jaspervdj.be/blaze/tutorial.html

painterGenerateGUI taskId Nothing iworld  
	= painterGenerateGUI taskId (Just {tool = "P", color = "black", mouseDown = Nothing, draw = [], lastDraw = Nothing, finished = False}) iworld

painterGenerateGUI _ (Just defSt) iworld  

	# ws = toString canvasWidth
	# hs = toString canvasHeight
	
	# canvas = 
			DivTag [StyleAttr "position: relative; float: left;"] [
					CanvasTag [IdAttr "canvas", WidthAttr ws, HeightAttr hs, StyleAttr "border: 1px solid #000;"] [],
					CanvasTag [IdAttr "tempcanvas", WidthAttr ws, HeightAttr hs, StyleAttr "position: absolute; top: 1px; left: 1px;"] []
					]

	# editor = [
			DivTag [StyleAttr "float: right;"] [
					DivTag [IdAttr "selectorYellow", StyleAttr "border-style:solid; border-color:white; background-color:yellow; width: 40px; height:40px; margin: 5px;"] [],
					DivTag [IdAttr "selectorRed", StyleAttr "border-style:solid; border-color:white; background-color:red; width: 40px; height: 40px; margin: 5px;"] [],
					DivTag [IdAttr "selectorGreen", StyleAttr "border-style:solid; border-color:white; background-color:green; width: 40px; height: 40px; margin: 5px;"] [],
					DivTag [IdAttr "selectorBlue", StyleAttr "border-style:solid; border-color:white; background-color:blue; width: 40px; height: 40px; margin: 5px;"] [],
					DivTag [IdAttr "selectorBlack", StyleAttr "border-style:solid; border-color:pink; background-color:black; width: 40px; height: 40px; margin: 5px;"] []
					],
			DivTag [StyleAttr "clear: both"] [],
			DivTag [] ([
					Text "Tool: ",
					SelectTag [IdAttr "selecttool"] [
						OptionTag [ValueAttr "P"] [Text "Pencil"],
						OptionTag [ValueAttr "L"] [Text "Line"],
						OptionTag [ValueAttr "R"] [Text "Rectangle"],
						OptionTag [ValueAttr "r"] [Text "Rectangle (filled)"],
						OptionTag [ValueAttr "C"] [Text "Circle"],
						OptionTag [ValueAttr "c"] [Text "Circle (filled)"]],
					ButtonTag [IdAttr "clearbtn", TypeAttr "button"] [Text "Clear"],
					ButtonTag [IdAttr "finishbtn", TypeAttr "button"] [Text "Finish"]
					])
			]
					
	# html = DivTag [StyleAttr "width: 360px; margin-right: auto;"] [canvas:editor]		

	# eventHandlers = [
			ComponentEvent "tasklet" "init" onStart,
			ComponentEvent "selectorYellow" "click" (onSelectColor "yellow"),
			ComponentEvent "selectorRed" "click" (onSelectColor "red"),
			ComponentEvent "selectorGreen" "click" (onSelectColor "green"),
			ComponentEvent "selectorBlue" "click" (onSelectColor "blue"),
			ComponentEvent "selectorBlack" "click" (onSelectColor "black"),
			ComponentEvent "canvas" "mousedown" onMouseDown,
			ComponentEvent "tempcanvas" "mousedown" onMouseDown,			
			ComponentEvent "canvas" "mouseup" onMouseUp,
			ComponentEvent "tempcanvas" "mouseup" onMouseUp,			
			ComponentEvent "canvas" "mousemove" onMouseMove,
			ComponentEvent "tempcanvas" "mousemove" onMouseMove,
			ComponentEvent "selecttool" "change" onChangeTool,
			ComponentEvent "clearbtn" "click" onClickClear,
			ComponentEvent "finishbtn" "click" onClickFinish]

	# gui = { width  		= ExactSize (canvasWidth + 70)
			, height 		= ExactSize (canvasHeight + 50)
			, html   		= html
			, eventHandlers = eventHandlers
			}
			
	= (TaskletHTML gui, defSt, iworld)

where
	onStart _ e state world
		# (context, world) = getContext False world
		# world = foldl (\world dr = draw context dr world) world (reverse state.draw)
		= (state, world)
		
	onChangeTool _ e state world
		# (selectedIndex, world) = jsGetObjectAttr "target.selectedIndex" e world
		# (options, world) 		 = jsGetObjectAttr "target.options" e world		
		# (option, world) 		 = jsGetObjectEl (jsValToInt selectedIndex) options world
		# (atool, world) 		 = jsGetObjectAttr "value" option world
		= ({state & tool = jsValToString atool}, world)	

	onSelectColor color _ e state world
		# world = foldl (\world el = setDomAttr el "style.borderColor" (toJSVal "white") world) world
					["selectorYellow","selectorRed","selectorGreen","selectorBlue","selectorBlack"]
		# (target, world) = jsGetObjectAttr "target" e world
		# world = jsSetObjectAttr "style.borderColor" (toJSVal "pink") target world
		= ({state & color = color}, world)

	getCoordinates e world
	    # (x, world) = jsGetObjectAttr "layerX" e world
	    # (y, world) = jsGetObjectAttr "layerY" e world
	    = ((jsValToInt x, jsValToInt y), e, world)

	onMouseDown _ e state world
	    # (coords, e, world) = getCoordinates e world
		= ({state & mouseDown = Just coords, lastDraw = Nothing}, world)

	getCanvas temp world
		= case temp of
			True = getDomElement "tempcanvas" world
			_	 = getDomElement "canvas" world

	getContext temp world
	 	# (canvas, world) = getCanvas temp world
		# (context, world) = callObjectMethod "getContext" [toJSArg "2d"] canvas world // not "2D" !
		= (context, world)

	clearContext context world
		# (_, world) = callObjectMethod "clearRect" 
							[toJSArg 0, toJSArg 0, toJSArg canvasWidth, toJSArg canvasHeight] context world
		= world

	onMouseUp _ e state world
		# (tempcanvas, world)  	= getCanvas True world
		# (tempcontext, world) 	= getContext True world
		# (context, world)     	= getContext False world
		# (_, world)  		  	= callObjectMethod "drawImage" [toJSArg tempcanvas, toJSArg 0, toJSArg 0] context world
		# world 			   	= clearContext tempcontext world
		| isJust state.lastDraw
			= ({state & mouseDown = Nothing, draw = [fromJust state.lastDraw:state.draw], lastDraw = Nothing}, world)
			= ({state & mouseDown = Nothing}, world)

	// generate onDrawing event
	onMouseMove _ e state world
		= case state.mouseDown of
			Just coord = onDrawing coord e state world
			_          = (state, world)
				
	drawLine context color x1 y1 x2 y2 world
		# (_, world) = callObjectMethod "beginPath" [] context world
		# world	     = jsSetObjectAttr "strokeStyle" (toJSVal color) context world
		# (_, world) = callObjectMethod "moveTo" [toJSArg x1, toJSArg y1] context world
		# (_, world) = callObjectMethod "lineTo" [toJSArg x2, toJSArg y2] context world
		# (_, world) = callObjectMethod "stroke" [] context world
		= world

	drawRect context color x1 y1 x2 y2 world
		# world		 = jsSetObjectAttr "strokeStyle" (toJSVal color) context world
		# (_, world) = callObjectMethod "strokeRect" 
						[toJSArg x1, toJSArg y1, toJSArg (x2 - x1), toJSArg (y2 - y1)]
						context world
		= world

	drawFilledRect context color x1 y1 x2 y2 world
		# world   	 = jsSetObjectAttr "fillStyle" (toJSVal color) context world
		# (_, world) = callObjectMethod "fillRect"
							[toJSArg x1, toJSArg y1, toJSArg (x2 - x1), toJSArg (y2 - y1)]
							context world
		= world

	drawCircle context fill color x1 y1 x2 y2 world
		# (_, world) = callObjectMethod "beginPath" [] context world
		# world		 = jsSetObjectAttr "strokeStyle" (toJSVal color) context world
		# world		 = jsSetObjectAttr "fillStyle" (toJSVal color) context world
	
		# (_, world) = callObjectMethod "arc"
							[toJSArg (center x1 x2), toJSArg (center y1 y2)
							,toJSArg (toInt ((distance x1 y1 x2 y2)/2.0))
							,toJSArg 0, toJSArg (3.14159265*2.0), toJSArg True]
							context world
							
		# (_, world) = case fill of 
						True = callObjectMethod "fill" [] context world
						_	 = callObjectMethod "stroke" [] context world

		# (_, world) = callObjectMethod "closePath" [] context world
		= world
	where
		center x1 x2 = (max x1 x2) - (abs (x1 - x2))/2
		distance x1 y1 x2 y2 = sqrt (toReal ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)))
				
	onDrawing (dx,dy) e state world
	    # ((x, y), e, world) = getCoordinates e world
		# (tempcontext, world) = getContext True world
			
		// Don't clear for pencil
		# world = case state.tool of 
				"P" = world
				    = clearContext tempcontext world

		# drawType = case state.tool of	
				"P" = DrawLine state.color dx dy x y
				"L"	= DrawLine state.color dx dy x y
				"R"	= DrawRect state.color False dx dy x y
				"r"	= DrawRect state.color True dx dy x y
				"C"	= DrawCircle state.color False dx dy x y
				"c"	= DrawCircle state.color True dx dy x y

		# world = draw tempcontext drawType world
			
		// Update start coordinates for pencil
		= case state.tool of 
				"P" = ({state & mouseDown = Just (x,y), draw=[drawType:state.draw], lastDraw = Nothing}, world)
				_   = ({state & lastDraw = Just drawType}, world)

	draw context (DrawLine color x1 y1 x2 y2) world
		= drawLine context color x1 y1 x2 y2 world
	draw context (DrawRect color False x1 y1 x2 y2) world
		= drawRect context color x1 y1 x2 y2 world
	draw context (DrawRect color True x1 y1 x2 y2) world
		= drawFilledRect context color x1 y1 x2 y2 world
	draw context (DrawCircle color fill x1 y1 x2 y2) world
		= drawCircle context fill color x1 y1 x2 y2 world

	onClickClear _ e state world
		# (context, world) = getContext False world
		# world = clearContext context world
		= ({state & draw = []}, world)
			
	onClickFinish _ e state world
		= ({state & finished = True}, world)

//-------------------------------------------------------------------------

taskletExamples :: [Workflow]
taskletExamples =
	[workflow "Simple push button tasklet" "Push the button 3(three) times" tasklet1,
	 workflow "Painter tasklet" "Simple painter tasklet" tasklet2,
	 workflow "GEO location tasklet" "GEO location tasklet" tasklet3]

tasklet1 :: Task Int
tasklet1
	= 		mkTask pushTasklet
		>>* [ OnAction ActionOk (ifValue (\n -> n >= 3))
            ] 

tasklet2 :: Task Drawing
tasklet2
	= 		mkTask painterTasklet
		>>* [ OnValue ifStable 
			] 

			
tasklet3 :: Task (Maybe GPSCoord)
tasklet3
	= 		mkTask geoTasklet
		>>* [ OnAction ActionOk (ifValue isJust),
		  	  OnAction ActionCancel (\_ = Nothing)
            ] 
							 
ifValue pred (Value v _) | pred v
	= Just (return v)
	= Nothing


ifStable (Value v True) = Just (return v)
ifStable _				= Nothing

returnC :: b (TaskValue a) -> Task b | iTask b
returnC v _ = return v

returnV :: (TaskValue a) -> Task a | iTask a
returnV (Value v _) = return v
    
Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist taskletExamples)) world
