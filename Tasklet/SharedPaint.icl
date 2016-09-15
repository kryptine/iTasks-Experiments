module SharedPaint

import iTasks.API.Core.Client.Editlet
import iTasks.API.Core.Client.Interface
from StdArray import class Array(uselect), instance Array {} a

import Text

:: Tool  = TLine | TRect | TRectF | TCircle | TCircleF
:: Color = Yellow | Red | Green | Blue | Black

colors = [Yellow, Red, Green, Blue, Black]
tools  = [TLine, TRect, TRectF, TCircle, TCircleF]

canvasWidth :== 300
canvasHeight :== 300

instance toString Color
where
  toString Yellow = "yellow"
  toString Red = "red"  
  toString Green = "green"    
  toString Blue = "blue"
  toString Black = "black"      

instance toString Tool
where
  toString TLine = "Line"
  toString TRect = "Rectangle"  
  toString TRectF = "Rectangle (filled)"    
  toString TCircle = "Circle"
  toString TCircleF = "Circle (filled)"     

instance fromString Tool
where
  fromString "Line" = TLine
  fromString "Rectangle" = TRect
  fromString "Rectangle (filled)" = TRectF    
  fromString "Circle" = TCircle  
  fromString "Circle (filled)" = TCircleF

:: PainterState = 	{ selectedTool  :: Tool
					, selectedColor :: Color
					, currentOrigin :: Maybe (Int, Int)
					, currentShape  :: Maybe Shape
					}

:: Filled :== Bool

:: Shape = Line !Color !Int !Int !Int !Int
		 | Rect !Color !Filled !Int !Int !Int !Int
		 | Circle !Color !Filled !Int !Int !Int !Int

:: Drawing = Drawing [Shape]

instance toString Shape
where
	toString (Line color a b c d) = "DrawLine " +++ toString color +++ " " +++toString a +++ " " +++ toString b +++ " " +++ toString c +++ " " +++ toString d 
	toString (Rect color filled a b c d) = "DrawRect " +++ toString color +++ " " +++ toString filled +++ " " +++ toString a +++ " " +++ toString b +++ " " +++ toString c +++ " " +++ toString d 
	toString (Circle color filled a b c d) = "DrawCircle " +++ toString color +++ " " +++ toString filled +++ " " +++ toString a +++ " " +++ toString b +++ " " +++ toString c +++ " " +++ toString d 

dumpDrawing :: [Shape] -> String
dumpDrawing drawing = join "," (map toString drawing)

derive class iTask PainterState, Shape, Color, Tool

derive gDefault Drawing
derive gText Drawing
derive gEq Drawing
derive gVerify Drawing
derive gEditMeta Drawing
derive JSONEncode Drawing
derive JSONDecode Drawing

withEditlet editlet dp (val,mask,ver) meta vst
	= gEditor{|*|} dp ({editlet & currVal = val},mask,ver) meta vst

gEditor{|Drawing|} dp vv meta env = withEditlet painterEditlet dp vv meta env
//gEditor{|Drawing|} = withEditlet painterEditlet

withEditlet2 editlet dp upd (val,mask) env
    # ((editlet,mask),env) = gUpdate{|*|} dp upd ({editlet & currVal = val},mask) env
    = ((editlet.currVal,mask),env) 

gUpdate{|Drawing|} dp upd vv iworld = withEditlet2 painterEditlet dp upd vv iworld
//gUpdate{|Drawing|} = withEditlet2 painterEditlet

painterEditlet :: Editlet Drawing [Shape] PainterState
painterEditlet
  = { Editlet
    | currVal   = Drawing []
    , defValSrv = Drawing []
    
    , genUI     = painterGUI
	, initClient = onInitUI
	, appDiffClt = updateUI    
	, genDiffSrv = srvGenDiff
	, appDiffSrv = \ns (Drawing ds) -> Drawing (ds ++ ns)
    }
where
	srvGenDiff (Drawing ds1) (Drawing ds2)
		= case drop (length ds1) ds2 of
			[] = Nothing
			shapes = Just shapes

	updateUI mkHandler cid ds cl world
		# (context, world) = getContext cid "pcanvas" world
		= (cl, foldl (\world dr = draw context dr world) world ds)

onInitUI mkHandler cid world
	# world = foldl addEventListener world eventHandlers
	= ({selectedTool = TLine, selectedColor = Black, currentOrigin = Nothing, currentShape = Nothing}, world)
where
	addEventListener world (ComponentEvent elementId event handler)
	 	= ((getElementById elementId) .# "addEventListener" .$! (event, mkHandler handler cid)) world

	mkId color = "sel_" +++ toString color +++ "_" +++ cid

	eventHandlers = map (\color -> ComponentEvent (mkId color) "click" (onSelectColor color)) colors ++ [
			ComponentEvent ("tcanvas_"+++cid) "mousedown" onMouseDown,			
			ComponentEvent ("tcanvas_"+++cid) "mouseup" onMouseUp,			
			ComponentEvent ("tcanvas_"+++cid) "mousemove" onMouseMove,
			ComponentEvent ("tool_"+++cid) "change" onChangeTool]
	
	onChangeTool :: ComponentId {JSObj JSEvent} PainterState *JSWorld -> *(!PainterState, !ComponentDiff [Shape] PainterState, !*JSWorld)
	onChangeTool _ {[0]=e} state world
		# (selectedIndex, world) = .? (e .# "target" .# "selectedIndex") world
		# (options, world) 		 = .? (e .# "target" .# "options") world		
		# (tool, world) 		 = .? (options .# jsValToInt selectedIndex .# "value") world
		= ({state & selectedTool = fromString (jsValToString tool)}, NoDiff, world)	

	onSelectColor :: Color ComponentId {JSObj JSEvent} PainterState *JSWorld -> *(!PainterState, !ComponentDiff [Shape] PainterState, !*JSWorld)
	onSelectColor color _ {[0]=e} state world
		# world = foldr (setBorder "white") world allBoxes
		# world = setBorder "pink" (e .# "target") world
		= ({state & selectedColor = color}, NoDiff, world)
	where
		allBoxes = map (getElementById  o mkId) colors
		setBorder color el world
			= (el .# "style" .# "borderColor" .= color) world
			
	getCoordinates e world
	    # (x, world) = .? (e .# "layerX") world
	    # (y, world) = .? (e .# "layerY") world
	    = ((jsValToInt x, jsValToInt y), world)

	onMouseDown _ {[0]=e} state world
	    # (coordinates, world) = getCoordinates e world
		= ({state & currentOrigin = Just coordinates}, NoDiff, world)

	onMouseUp _ {[0]=e} state world
		# (tempcontext, world) 	= getContext cid "tcanvas" world	
		# world 			   	= clearCanvas tempcontext world
		
		# (diff, world) = case state.currentShape of
			Just shape 
				# (context, world) = getContext cid "pcanvas" world
				= (addShape shape, draw context shape world)
			Nothing
				= (NoDiff, world)
				
		= ({state & currentOrigin = Nothing, currentShape = Nothing}, diff, world)
	
	addShape shape = Diff [shape] callback
	where
		callback True state world 
			# (context, world) 	= getContext cid "pcanvas" world
			= (state, addShape shape, draw context shape world)
		callback False state world 
			= (state, NoDiff, world)
	
	// generate onDrawing event
	onMouseMove _ {[0]=e} state world
		= case state.currentOrigin of
			Just coordinates = onDrawing coordinates e state world
			_                = (state, NoDiff, world)

	onDrawing (ox, oy) e state world
    	# ((x, y), world) = getCoordinates e world
		# (tempcontext, world) = getContext cid "tcanvas" world
			
		# world = clearCanvas tempcontext world

		# currentShape = case state.selectedTool of	
			TLine	 = Line state.selectedColor ox oy x y
			TRect	 = Rect state.selectedColor False ox oy x y
			TRectF	 = Rect state.selectedColor True ox oy x y
			TCircle	 = Circle state.selectedColor False ox oy x y
			TCircleF = Circle state.selectedColor True ox oy x y

		# world = draw tempcontext currentShape world
			
		= ({state & currentShape = Just currentShape}, NoDiff, world)


:: JSCanvasContext = JSCanvasContext
:: Context :== JSVal JSCanvasContext

getContext :: ComponentId String *JSWorld -> *(Context, *JSWorld)
getContext cid canvasId world
 	# canvas = getElementById (canvasId +++ "_" +++ cid)
	= (canvas .# "getContext" .$ ("2d")) world // not "2D" !

clearCanvas :: Context *JSWorld -> *JSWorld
clearCanvas context world
	= (context .# "clearRect" .$! (0, 0, canvasWidth, canvasHeight)) world
			
drawLine :: Context Color Int Int Int Int *JSWorld -> *JSWorld			
drawLine context color x1 y1 x2 y2 world
	# world = (context .# "beginPath" .$! ()     ) world
	# world = (context .# "strokeStyle" .= color ) world
	# world = (context .# "moveTo" .$! (x1, y1)  ) world
	# world = (context .# "lineTo" .$! (x2, y2)  ) world
	# world = (context .# "stroke" .$! ()        ) world
	= world

drawRect :: Context Color Filled Int Int Int Int *JSWorld -> *JSWorld
drawRect context color False x1 y1 x2 y2 world
	# world = (context .# "strokeStyle" .= color                     ) world
	# world = (context .# "strokeRect" .$! (x1, y1, x2 - x1, y2 - y1)) world
	= world

drawRect context color True x1 y1 x2 y2 world
	# world = (context .# "fillStyle" .= color					   ) world
	# world = (context .# "fillRect" .$! (x1, y1, x2 - x1, y2 - y1)) world
	= world

drawCircle :: Context Color Filled Int Int Int Int *JSWorld -> *JSWorld
drawCircle context color filled x1 y1 x2 y2 world
	# world = (context .# "beginPath" .$! ()      ) world
	# world = (context .# "strokeStyle" .= color  ) world
	# world = (context .# "fillStyle" .= color    ) world
	
	# world = (context .# "arc" .$! (center x1 x2, 
									 center y1 y2, 
									 toInt ((distance x1 y1 x2 y2)/2.0),
									 0, 
									 3.14159265*2.0, 
									 True)) world
							
	# world  = case filled of 
					True = (context .# "fill" .$! ()) world
					_	 = (context .# "stroke" .$! ()) world

	= (context .# "closePath" .$! ()) world
where
	center x1 x2 = (max x1 x2) - (abs (x1 - x2))/2
	distance x1 y1 x2 y2 = sqrt (toReal ((x1 - x2)*(x1 - x2) + (y1 - y2)*(y1 - y2)))
				
draw :: Context Shape *JSWorld -> *JSWorld
draw context (Line color x1 y1 x2 y2) world
	= drawLine context color x1 y1 x2 y2 world
draw context (Rect color filled x1 y1 x2 y2) world
	= drawRect context color filled x1 y1 x2 y2 world
draw context (Circle color filled x1 y1 x2 y2) world
	= drawCircle context color filled x1 y1 x2 y2 world

// TODO: http://jaspervdj.be/blaze/tutorial.html
// http://stackoverflow.com/questions/18201257/drawing-a-rectangle-without-clearing-the-canvas-eventlisteners

:: PainterEventHand :== ComponentId {JSObj JSEvent} PainterState *JSWorld -> *(!PainterState, !ComponentDiff [Shape] PainterState, !*JSWorld)

painterGUI :: ComponentId *World -> *(EditletHTML, *World)
painterGUI cid world  

	# ws = toString canvasWidth
	# hs = toString canvasHeight
	
	# canvas = 
			DivTag [StyleAttr "position: relative; float: left;"] [
					CanvasTag [IdAttr ("pcanvas_"+++cid), WidthAttr ws, HeightAttr hs, StyleAttr "border: 1px solid #000;"] [],
					CanvasTag [IdAttr ("tcanvas_"+++cid), WidthAttr ws, HeightAttr hs, StyleAttr "position: absolute; top: 1px; left: 1px;"] []
					]

	# editor = [
			DivTag [StyleAttr "float: right;"] selectors,
			DivTag [StyleAttr "clear: both;"] [],
			DivTag [] [Text "Tool: ",SelectTag [IdAttr ("tool_"+++cid)] optionTags]
			]
					
	# html = DivTag [StyleAttr "width: 360px; margin-right: auto;"] [canvas:editor]		

	# gui = {ComponentHTML
			| width  		= ExactSize (canvasWidth + 70)
			, height 		= ExactSize (canvasHeight + 50)
			, html   		= html
			}
			
	= (gui, world)
where
	optionTags = map (\tool -> let label = toString tool in OptionTag [ValueAttr label] [Text label]) tools
		
	selectors = map (\color -> DivTag [IdAttr (mkId color), 
			StyleAttr ("border-style:solid; border-color:white; background-color:" +++ toString color +++ "; width: 40px; height:40px; margin: 5px;")] []) colors

	mkId color = "sel_" +++ toString color +++ "_" +++ cid


//-------------------------------------------------------------------------

import StdMisc

editletExamples :: [Workflow]
editletExamples =
	[workflow "Painter tasklet" "Simple painter tasklet" painter]

painter :: Task Drawing
painter = withShared (Drawing []) (\drawing -> updateSharedInformation "1" [] drawing
													-|| 
									           updateSharedInformation "1" [] drawing)
		    
Start :: *World -> *World
Start world = startEngine (workAs (AuthenticatedUser "root" [] Nothing) (manageWorklist editletExamples)) world
