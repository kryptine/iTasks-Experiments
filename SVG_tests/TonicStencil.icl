module TonicStencil

import StdList
from StdFunc import seqList, :: St
import Graphics.Scalable
import Data.Maybe
import qualified Data.Map as DM
import qualified Data.Set as DS
import iTasks
import iTasks.API.Extensions.SVG.SVGlet

ArialRegular10px :== { fontfamily  = "Arial"
                     , fontysize   = 10.0
                     , fontstretch = "normal"
                     , fontstyle   = "normal"
                     , fontvariant = "normal"
                     , fontweight  = "normal"
                     }

ArialBold10px :== { fontfamily  = "Arial"
                  , fontysize   = 10.0
                  , fontstretch = "normal"
                  , fontstyle   = "normal"
                  , fontvariant = "normal"
                  , fontweight  = "bold"
                  }

ArialItalic10px :== { fontfamily = "Arial"
                  , fontysize    = 10.0
                  , fontstretch  = "normal"
                  , fontstyle    = "italic"
                  , fontvariant  = "normal"
                  , fontweight   = "normal"
                  }

//Start :: *World -> *World
Start world = startEngine examples world
//Start world = startEngine simpleClickExample world
//Start world = startEngine allSVGs world
//Start world = startEngine viewScaleExample world

//Start = fst (fixSpans viewTaskDefExample` {srvTaggedSpanEnv = 'DM'.newMap, didChange = False, srvCounter = 0, srvFonts = 'DM'.newMap})

viewImage d image = viewInformation d [imageView (\_ -> image)] ()


//viewTaskDefExample` = viExample
  //where
  //viExample   = tTaskDef "logCall" "Emergency" [("now", "DateTime")] bodyImage
  //eiApp       = tTaskApp "enterInformation" ["\"Enter call information:\"", "[]"]
  //meApp       = tTransformApp "makeEmergency" ["now", "data"]
  //startSymb   = polygon Nothing [ (px 0.0, px 0.0), (px 16.0, px 8.0), (px 0.0, px 16.0) ]
  //lineArrow   = polygon Nothing [ (px 0.0, px 0.0), (px 8.0, px 4.0), (px 0.0, px 8.0) ]
  //stopSymb    = rect (px 16.0) (px 16.0)
  //bodyImage   = grid (Rows 1) (LeftToRight, TopToBottom) (repeat (AtMiddleX, AtMiddleY)) [] [startSymb, hline, eiApp, hline, meApp, hline, stopSymb] Nothing
  //hline       = xline linemarkers (px 50.0)
  //linemarkers = Just { markerStart = Nothing
                     //, markerMid   = Nothing
                     //, markerEnd   = Just lineArrow
                     //}

examples :: Task ()
examples = viewInformation () [] "Select an example set"
  >>* [ // OnAction (Action "Various static examples" []) (always (allSVGs @! ()))
      //, OnAction (Action "Static Tonic shapes" [])     (always (tonicSVGs @! ()))
       OnAction (Action "On-click example" [])        (always (allClickExamples @! ()))
      //, OnAction (Action "Hello, world!" [])           (always (helloWorldExample @! ()))
      //, OnAction (Action "Grid layout + offsets" [])   (always (gridLayoutPlusOffsets @! ()))
      //, OnAction (Action "above test" [])   (always (aboveTest @! ()))
      ]

//aboveTest = viewImage "no wrap" imgNoWrap -&&- viewImage "wrap" imgWrap
  //where
  //img = rect (px 100.0) (px 100.0)
  //imgNoWrap = overlay (repeat (AtMiddleX, AtMiddleY)) [] [img] Nothing
  //imgWrap = overlay (repeat (AtMiddleX, AtMiddleY)) [] [(above [AtMiddleX] [] [img] Nothing)] Nothing

//gridLayoutPlusOffsets = viewImage "Grid layout + offsets" offsetImg
  //where
  //offsetImg = beside (repeat AtTop) [(px 0.0, px -10.0), (px 10.0, px 0.0)] [imgA, imgB] Nothing
  //imgA = rect (px 50.0) (px 50.0)
  //imgB = rect (px 100.0) (px 100.0)

//// TODO : bounding boxes of rotated images aren't calculated correctly yet
//viewRotateGridImg = viewImage "Rotate test" rotateGridImg
//rotateGridImg
  //# rect1 = rotate (deg 45.0) (rect (px 50.0) (px 50.0) <@< { fill = toSVGColor "red" })
  //# rect2 = rotate (deg 45.0) (rect (px 50.0) (px 50.0) <@< { fill = toSVGColor "red" })
  //= collage ([(px n, px m) \\ n <- [0.0, 50.0, 100.0], m <- [0.0, 50.0, 100.0]] ++ [(px 0.0, px 0.0), (px 50.0, px 50.0)]) (repeatn 9 (rect (px 5.0) (px 5.0)) ++ [rect1, rect2]) Nothing

  ////<rect width="5" height="5" transform="translate(0, 0)" />
  ////<rect width="5" height="5" transform="translate(50, 0)" />
  ////<rect width="5" height="5" transform="translate(100, 0)" />

  ////<rect width="5" height="5" transform="translate(0, 50)" />
  ////<rect width="5" height="5" transform="translate(50, 50)" />
  ////<rect width="5" height="5" transform="translate(100, 50)" />

  ////<rect width="5" height="5" transform="translate(0, 100)" />
  ////<rect width="5" height="5" transform="translate(50, 100)" />
  ////<rect width="5" height="5" transform="translate(100, 100)" />

  ////<g transform="translate(50, 50)">
  ////<rect width="50" height="50" style="fill:red;stroke:black;stroke-width:1;opacity:0.5" transform="rotate(45 25 25)" />
  ////</g>

//helloWorldExample = viewImage "Hello, world!" (text ArialRegular10px "Hello, world!")

//viewTestRect :: Task ()
//viewTestRect = viewImage "Test rect" testRect

//testRect :: Image ()
//testRect = rect (textxspan ArialRegular10px "foo") (px 25.0)

////traxtest = viewImage "Trax" (above [] [] [ve_img,ho_img,nw_img,ne_img,se_img,sw_img] Nothing)
//traxtest = viewImage "Trax" (above [] [] [ve_img, empty (px 75.0) (px 75.0), ho_img, empty (px 75.0) (px 75.0),nw_img, empty (px 75.0) (px 75.0),ne_img, empty (px 75.0) (px 75.0),se_img, empty (px 75.0) (px 75.0),sw_img] Nothing)
////traxtest = allTasks [viewImage label img \\ (label,img) <- [("vertical",ve_img),("horizontal",ho_img),("northwest",nw_img),("northeast",ne_img),("southeast",se_img),("southwest",sw_img)]]
//where
	//ve_img	= overlay [(AtMiddleX,AtMiddleY),(AtMiddleX,AtMiddleY)] [] [ xl "white", yl "red"] tile
	//ho_img	= overlay [(AtMiddleX,AtMiddleY),(AtMiddleX,AtMiddleY)] [] [ yl "white", xl "red"] tile
	//nw_img	= overlay [] [(d /.  2, d /.  2),(d /. -2,d /. -2)] [ arc "white", arc "red" ] tile
	//ne_img	= overlay [] [(d /. -2, d /.  2),(d /.  2,d /. -2)] [ arc "white", arc "red" ] tile
	//se_img	= overlay [] [(d /. -2, d /. -2),(d /.  2,d /.  2)] [ arc "white", arc "red" ] tile
	//sw_img	= overlay [] [(d /.  2, d /. -2),(d /. -2,d /.  2)] [ arc "white", arc "red" ] tile
	//d		= px 50.0
	//tile	= Just (rect d d)
	//arc c	= circle d <@< {stroke = toSVGColor c} <@< {strokewidth = d /. 5} <@< {fill = toSVGColor "none"}
	//xl  c   = xline Nothing d <@< {stroke = toSVGColor c} <@< {strokewidth = d /. 5}
	//yl  c	= yline Nothing d <@< {stroke = toSVGColor c} <@< {strokewidth = d /. 5}

////allSVGs = allTasks [ // traxtest @! ()
                     ////viewRotateGridImg @! ()
                   //////, viewLineExample @! ()
                   ////, viewScaleExample
                   ////, viewTextGrid @! ()
                   ////, viewTextGrid2 @! ()
                   //////, viewTextGrid3 @! ()
                   ////, viewPolygon @! ()
                   ////, viewPolyline @! ()
                   ////, viewBox @! ()
                   ////, viewGrid @! ()
                   ////, viewShapes mkCircles Nothing @! ()
                   ////, viewShapes mkCircles (Just rect100x60) @! ()
                   ////, viewShapes mkCircles (Just rect30x60) @! ()
                   ////, viewRect @! ()
                   ////, viewEllipse @! ()
                   ////, viewBesideBoxes @! ()
                   ////, viewBesideBoxes2 @! ()
                   //////, viewShapes mkRects Nothing @! ()
                   //////, viewShapes mkRects (Just rect100x60) @! ()
                   //////, viewShapes mkRects (Just rect30x60) @! ()
                   ////]
allClickExamples = allTasks [ // simpleClickExample @! ()
                            //, simpleClickExample` @! ()
                            //, simpleClickStepExample @! ()
                            doubleClickExample @! ()
                            ]
//:: ClickStepSt
  //= { bigRectClicked   :: Bool
    //, smallRectClicked :: Bool
    //}

//derive class iTask ClickStepSt

//viewScaleExample :: Task ()
//viewScaleExample = viewImage "Scaled rect" (fitx (px 50.0) (rect (px 100.0) (px 100.0)))

////simpleClickStepExample = updateImageState "Clickable boxes" defaultState mkBoxes >>* [OnValue doStep]
  ////where
  ////defaultState = { bigRectClicked = False, smallRectClicked = False }
  ////mkBoxes s = beside [] [] [ rect100x60 <@< { onclick = \st -> { st & bigRectClicked = True }}
                           ////, rect30x60  <@< { onclick = \st -> { st & smallRectClicked = True }}] Nothing
  ////doStep (Value {bigRectClicked   = True} _) = Just (viewInformation () [] "Big rect clicked!")
  ////doStep (Value {smallRectClicked = True} _) = Just (viewInformation () [] "Small rect clicked!")
  ////doStep _           = Nothing


////simpleClickStepExample2 = updateImageState "Clickable boxes 2" defaultState mkBoxes
                      ////>>* [ OnImageClick [imageTag "bigbox"]   (\n -> viewInformation "bigbox n"   [] n)
                          ////, OnImageClick [imageTag "smallbox"] (\n -> viewInformation "smallbox n" [] n) ]
  ////where
  ////defaultState = 42
  ////mkBoxes s = beside [] [] [ tag [imageTag "bigbox"]   rect100x60 <@< { onclick = \st -> st + 1}
                           ////, tag [imageTag "smallbox"] rect30x60  <@< { onclick = \st -> st - 1}] Nothing

////OnImageClick tags f = OnValue mkOnValue
  ////where
  ////mkOnValue (Value (s, Just lastClickedTags) _)
    ////| 'DS'.isSubsetOf ('DS'.fromList tags) lastClickedTags = Just (f s)
  ////mkOnValue _ = Nothing

//tonicSVGs = allTasks [ viewTaskAppExample @! ()
                     //, viewStartExample @! ()
                     //, viewStopExample @! ()
                     //, viewStepStarExample @! ()
                     //, viewTaskDefExample @! ()
                     //]

//rect100x60 = rect (px 100.0) (px 60.0) <@< { fill = toSVGColor "red" }
//rect30x60 = rect (px 30.0) (px 60.0) <@< { fill = toSVGColor "blue" }

//viewLineExample = viewImage "Testing lines" mkImg
  //where
  //mkImg = overlay (repeat (AtMiddleX, AtMiddleY)) [(px 50.0, px 50.0), (px 150.0, px 150.0)] [rect100x60, rect30x60] Nothing

//simpleClickExample :: Task Real
//simpleClickExample = viewInformation "Simple click example" [imageView f] 25.0
  //where
  //f :: Real -> Image Real
  //f n = rect (px n) (px n) <@< { onclick = \r -> r + 25.0 }

doubleClickExample :: Task Real
doubleClickExample = viewInformation "Simple doubleclick example" [imageView f] 25.0
  where
  f :: Real -> Image Real
  f n = rect (px n) (px n) <@< { ondblclick = \r -> r + 25.0 }

//simpleClickExample` :: Task Real
//simpleClickExample` = updateInformation "Simple click example" [imageUpdate id f (\_ x -> x)] 25.0
  //where
  //f :: Real -> Image Real
  //f n = rect (px n) (px n) <@< { onclick = \r -> r + 25.0 }


//viewPolygon :: Task ()
//viewPolygon = viewImage "Star-shaped polygon"  mkpolygon
  //where
  //mkpolygon = polygon Nothing
                 //[ (px 100.0, px 10.0)
                 //, (px 40.0, px 198.0)
                 //, (px 190.0, px 78.0)
                 //, (px 10.0, px 78.0)
                 //, (px 160.0, px 198.0) ] <@< { fill   = toSVGColor "green" }
                                          //<@< { stroke = toSVGColor "black" }

//viewPolyline :: Task ()
//viewPolyline = viewImage "Stair-shaped polyline" mkpolyline
  //where
  //mkpolyline = overlay (repeat (AtMiddleX, AtMiddleY)) []
                 //[ rect (px 250.0) (px 250.0) <@< { fill = toSVGColor "white" }
                 //, polyline (Just { markerStart = Just (text ArialRegular10px "Start!")
                            //, markerMid   = Just (circle (px 20.0))
                            //, markerEnd   = Just (rect (px 20.0) (px 20.0))})
                 //[ (px 0.0, px 40.0)
                 //, (px 40.0, px 40.0)
                 //, (px 40.0, px 80.0)
                 //, (px 80.0, px 80.0)
                 //, (px 80.0, px 120.0)
                 //, (px 120.0, px 120.0)
                 //, (px 120.0, px 160.0)
                 //]] Nothing


//viewBesideBoxes :: Task ()
//viewBesideBoxes = viewImage "Two boxes besides eachother" boxes
  //where
  //boxes = beside [] [] [tag (imageTag "foo") rect100x60, rect30x60] Nothing

//viewBesideBoxes2 :: Task ()
//viewBesideBoxes2 = viewImage "Two boxes besides eachother. Second box looks up spans of the first"  besideBoxes2

//besideBoxes2 :: Image ()
//besideBoxes2 = beside [] [] [tag (imageTag "foo") rect100x60, rect (imagexspan (imageTag "foo")) (imageyspan (imageTag "foo"))] Nothing

//viewGrid :: Task ()
//viewGrid = viewImage "Grid with text, besides composition, above composition and variouse alignments" gridImg

////gridImg :: Image ()
////gridImg  = grid (Columns 3) (LeftToRight, TopToBottom) (repeat (AtLeft, AtTop)) []
    ////[ text ArialRegular10px "beside (repeat AtTop) [] cs Nothing",     beside (repeat AtTop) [] cs Nothing
    ////, text ArialRegular10px "above (repeat AtLeft) [] cs Nothing",     above  (repeat AtLeft) [] cs Nothing
    ////, text ArialRegular10px "beside (repeat AtMiddleY) [] cs Nothing", beside (repeat AtMiddleY) [] cs Nothing
    ////, text ArialRegular10px "above (repeat AtMiddleX) [] cs Nothing",  above  (repeat AtMiddleX) [] cs Nothing
    ////, text ArialRegular10px "beside (repeat AtBottom) [] cs Nothing",  beside (repeat AtBottom) [] cs Nothing
    ////, text ArialRegular10px "above (repeat AtRight) [] cs Nothing",    above  (repeat AtRight) [] cs Nothing
    ////] Nothing
  ////where
  ////cs  = [circle (px (toReal ((d+1)*10))) <@< {fill=toSVGColor {zero & g=135+d*30}} \\ d <- [4,3 .. 0]]

////viewTextGrid :: Task ()
////viewTextGrid = viewImage "Grid of text. No transformations." (textGridImg ArialRegular10px 4 2)

////textGridImg :: FontDef Int Int -> Image ()
////textGridImg font c r
  ////= grid (Rows (r + 1)) (LeftToRight, TopToBottom) (repeat (AtLeft,AtTop)) []
          ////(  [ empty (px 0.0) (px 0.0) : [ textbox font ("column " <+++ col) \\ col <- [1 .. c] ] ]
          ////++  flatten
                ////[[ (textbox font ("row " <+++ row)) : [ text font ("(" +++ toString row +++ ", " +++ toString col +++ ")") \\ col <- [1 .. c]]]
                ////\\ row <- [1 .. r]
                ////]
          ////) Nothing
  ////where
  ////textbox font txt
    ////= overlay [(AtMiddleX, AtTop)] [] [text font txt <@< {stroke=toSVGColor "white"}]
       ////(Just (rect (textxspan font (txt+++"MM")) (px font.fontysize)))

//viewTextGrid2 :: Task ()
//viewTextGrid2 = viewImage "Grid of text. Left-most column cells rotated -90 deg." (textGridImg2 ArialRegular10px 4 2)

//textGridImg2 :: FontDef Int Int -> Image ()
//textGridImg2 font c r
  //= grid (Rows (r + 1)) (LeftToRight, TopToBottom) (repeat (AtLeft,AtTop)) []
          //(  [ empty (px 0.0) (px 0.0) : [ textbox font ("column " <+++ col) \\ col <- [1 .. c] ] ]
          //++  flatten
                //[[ rotate (deg -90.0) (textbox font ("row " <+++ row)) : [ text font ("(" +++ toString row +++ ", " +++ toString col +++ ")") \\ col <- [1 .. c]]]
                //\\ row <- [1 .. r]
                //]
          //) Nothing
  //where
  //textbox font txt
    //= overlay [(AtMiddleX, AtTop)] [] [text font txt <@< {stroke=toSVGColor "white"}]
       //(Just (rect (textxspan font (txt+++"MM")) (px font.fontysize)))

//viewTextGrid3 :: Task ()
//viewTextGrid3 = viewImage "Grid with cell size lookups" (textGridImg3 ArialRegular10px 4 2)

//textGridImg3 :: FontDef Int Int -> Image ()
//textGridImg3 font c r
  //= tag ts (grid (Rows (r + 1)) (LeftToRight, TopToBottom) (repeat (AtLeft, AtTop)) []
             //([ empty (px 0.0) (px 0.0) : [ edgeCell ts (col + 1, 1) black ("column " <+++ col) id \\ col <- [1 .. c] ] ]
              //++  flatten
              //[ [ edgeCell ts (1, row + 1) black ("row " <+++ row) (rotate (deg -90.0))
                //: [ cell ts (col + 1, row + 1) (if (isOdd col) none grey) (textbox font (toString ("(" +++ toString row +++ ", " +++ toString col +++ ")"))) \\ col <- [1 .. c]]
                //]
              //\\ row <- [1 .. r]
              //]
             //) Nothing)
  //where
  //ts    = imageTag "table"
  //black = toSVGColor "black"
  //grey  = toSVGColor "grey"
  //none  = toSVGColor "none"
  //edgeCell ts (c,r) clr txt t = overlay [(AtMiddleX, AtMiddleY)] [] [textbox font txt] (Just (t (rect (textxspan font (txt+++"MM")) (px font.fontysize)) <@< {fill=clr}))
  //cell     ts (c,r) clr img   = overlay [(AtMiddleX, AtMiddleY)] [] [img] (Just (rect (columnspan ts c) (rowspan ts r) <@< {fill=clr}))
  //textbox font txt
    //= overlay [(AtMiddleX, AtTop)] [] [text font txt <@< {stroke=toSVGColor "white"}]
       //(Just (rect (textxspan font (txt+++"MM")) (px font.fontysize)))

////viewRose :: Task (Editlet () ((), Image ()))
////viewRose = viewInformation "Tonic SVG" [] (simpleSVGlet (fst (showRose f tree tags)))
  ////where
  ////f = undef
  ////tree = undef
  ////tags = undef

////undef = undef

////:: Rose a = RoseNode a [Rose a]

////showRose :: (a [ImageTag] -> (Image m, [ImageTag])) (Rose a) [ImageTag] -> (Image m, [ImageTag])
////showRose show_node (RoseNode r []) ts = show_node r ts
////showRose show_node (RoseNode r rs) [t1, t2 : ts]
  ////= ( above (repeat AtLeft) []
    ////(fst (show_node r [])
    ////, beside (repeat AtTop) []
      ////[ yline (imageyspan t1` - imageyspan t2`)
      ////, tag t1` (grid (Columns 2) (LeftToRight, TopToBottom) (repeat (AtLeft,AtTop)) [] (repeatn (length rs) (xline (px 10.0)) ++ init images ++ [tag t2` (last images)]) Nothing)
      ////] Nothing
    ////] Nothing, ts` )
  ////where
  ////t1` = 'DS'.singleton t1
  ////t2` = 'DS'.singleton t2
  ////(images, ts`) = seqList (map (showRose show_node) rs) ts


//viewRect :: Task ()
//viewRect = viewImage "A simple rectangle" rect100x60

//viewEllipse :: Task ()
//viewEllipse = viewImage "A simple ellipse" (ellipse (px 30.0) (px 100.0))

//viewBox :: Task ()
//viewBox = viewImage "A 2.5D box" box

//viewShapes :: (ImageAlign (Maybe (Image m)) -> Image ()) (Maybe (Image m)) -> Task ()
//viewShapes f host = allTasks (map (\x -> viewImage "Shapes overlaid on top of eachother with various alignments." (f x host)) aligns) @! ()
  //where
  //aligns =
    //[ (AtLeft, AtTop)
    //, (AtMiddleX, AtTop)
    //, (AtRight, AtTop)
    //, (AtLeft, AtMiddleY)
    //, (AtMiddleX, AtMiddleY)
    //, (AtRight, AtMiddleY)
    //, (AtLeft, AtBottom)
    //, (AtMiddleX, AtBottom)
    //, (AtRight, AtBottom)
    //]

//mkRects (xalign, yalign) host = overlay (repeat (xalign, yalign)) [] cs host
  //where
  //cs = [ rect (dim d) (dim d) <@< {fill = toSVGColor {zero & g = 135 + d * 30}}
       //\\ d <- [4,3 .. 0]]
  //dim d = px (toReal ((d+1)*10))

//mkCircles (xalign, yalign) host = overlay (repeat (xalign, yalign)) [] cs host
  //where
  //cs = [ circle (px (toReal ((d+1)*10))) <@< {fill = toSVGColor {zero & g = 135 + d * 30}}
       //\\ d <- [4,3 .. 0]]

//r1 = skewx (deg -45.0) (rect (px 200.0) (px 30.0))
//r2 = rect (px 200.0) (px 100.0) <@< { fill   = toSVGColor "slategrey" }
                                //<@< { stroke = toSVGColor "none" }
//r3 = skewy (deg -45.0) (rect (px 30.0) (px 100.0) <@< { fill   = toSVGColor "darkslategrey" }
                                                     //<@< { stroke = toSVGColor "none" })
//box :: Image ()
//box = collage [(px 30.0, zero), (zero, px 30.0), (px 200.0, px 30.0)] [r1, r2, r3] Nothing

//viewTaskAppExample :: Task ()
//viewTaskAppExample = viewImage "Tonic task-application render." viExample
  //where
  //viExample = tTaskApp "viewInformation" ["\"Please draw a lovely SVG image for me!\"", "[]", "lovelyimage.svg"]

//taskApp` :: String [String] -> Image ()
//taskApp` taskName taskArgs
  //# bgRect       = rect containerSpan (px ArialRegular10px.fontysize *. (2.5 + toReal (length taskArgs))) <@< { fill        = toSVGColor "white" }
                                                                                                       //<@< { stroke      = toSVGColor "black" }
                                                                                                       //<@< { strokewidth = px 1.0 }
                                                                                                       //<@< { xradius     = px 5.0 }
                                                                                                       //<@< { yradius     = px 5.0 }
  //# taskNameImg  = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ rect (textxspan ArialRegular10px (taskName +++ "MM")) (px (ArialRegular10px.fontysize + 5.0)) <@< { fill = toSVGColor "none" } <@< { stroke = toSVGColor "none"}
                                                              //, text ArialBold10px taskName
                                                              //] Nothing
  //# taskArgsImgs = overlay (repeat (AtMiddleX, AtMiddleY)) [] [ rect containerSpan (px (ArialRegular10px.fontysize * toReal (length taskArgs + 1))) <@< { fill = toSVGColor "none" } <@< { stroke = toSVGColor "none"}
                                                              //, above (repeat AtLeft) [] (map (text ArialRegular10px) taskArgs) Nothing
                                                              //] Nothing
  //# taskText     = above (repeat AtMiddleX) [] [taskNameImg, xline Nothing containerSpan, taskArgsImgs] Nothing
  //# taskApp      = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskText] Nothing
  //= taskApp
  //where
  //maxXSpan = maxSpan [textxspan ArialRegular10px taskName : map (textxspan ArialRegular10px) taskArgs]
  //containerSpan = maxXSpan + textxspan ArialRegular10px "MM"

//viewStartExample :: Task ()
//viewStartExample = viewImage "Tonic start symbol." img
  //where
  //img = polygon Nothing [ (px 0.0, px 0.0)
                        //, (px 16.0, px 8.0)
                        //, (px 0.0, px 16.0) ]

//viewStopExample :: Task ()
//viewStopExample = viewImage "Tonic stop symbol." img
  //where
  //img = rect (px 16.0) (px 16.0)

//viewStepStarExample :: Task ()
//viewStepStarExample = viewImage "Tonic step star symbol." img
  //where
  //img = overlay (repeat (AtMiddleX, AtMiddleY)) [] [diamond, star] Nothing
  //diamond = rotate (deg 45.0) (rect (px 16.0) (px 16.0)
              //<@< { fill   = toSVGColor "black" }
              //<@< { stroke = toSVGColor "none" })
  //star = polygon Nothing
                 //[ (px 5.0, px 0.0)
                 //, (px 2.0, px 10.0)
                 //, (px 9.5, px 4.0)
                 //, (px 0.0, px 4.0)
                 //, (px 8.0, px 10.0) ] <@< { fill   = toSVGColor "white" }
                                       //<@< { stroke = toSVGColor "none" }

//viewTaskDefExample :: Task ()
//viewTaskDefExample = viewImage "Tonic task-definition render." viExample
  //where
  //viExample   = tTaskDef "logCall" "Emergency" [("now", "DateTime")] bodyImage
  //eiApp       = tTaskApp "enterInformation" ["\"Enter call information:\"", "[]"]
  //meApp       = tTransformApp "makeEmergency" ["now", "data"]
  //startSymb   = polygon Nothing [ (px 0.0, px 0.0), (px 16.0, px 8.0), (px 0.0, px 16.0) ]
  //lineArrow   = polygon Nothing [ (px 0.0, px 0.0), (px 8.0, px 4.0), (px 0.0, px 8.0) ]
  //stopSymb    = rect (px 16.0) (px 16.0)
  //bodyImage   = grid (Rows 1) (LeftToRight, TopToBottom) (repeat (AtMiddleX, AtMiddleY)) [] [startSymb, hline, eiApp, hline, meApp, hline, stopSymb] Nothing
  //hline       = xline linemarkers (px 50.0)
  //linemarkers = Just { markerStart = Nothing
                     //, markerMid   = Nothing
                     //, markerEnd   = Just lineArrow
                     //}

//// Actual reusable tonic stencils

//tDiamond :: Image s
//tDiamond = rotate (deg 45.0) (rect (px 16.0) (px 16.0))
             //<@< { fill   = toSVGColor "black" }
             //<@< { stroke = toSVGColor "none" }

//tStepStar :: Image s
//tStepStar = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, star] Nothing
  //where
  //star = polygon Nothing
           //[ (px 5.0, px 0.0)
           //, (px 2.0, px 10.0)
           //, (px 9.5, px 4.0)
           //, (px 0.0, px 4.0)
           //, (px 8.0, px 10.0) ] <@< { fill   = toSVGColor "white" }
                                 //<@< { stroke = toSVGColor "none" }

//tParSum :: Image s
//tParSum = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, plus] Nothing
  //where
  //plus = overlay (repeat (AtMiddleX, AtMiddleY)) [] [line xline, line yline] Nothing
  //line f = f Nothing (px 10.0) <@< {stroke = toSVGColor "white"} <@< {strokewidth = px 2.0}

//tParProd :: Image s
//tParProd = overlay (repeat (AtMiddleX, AtMiddleY)) [] [tDiamond, plus] Nothing
  //where
  //plus = rotate (deg 45.0) (overlay (repeat (AtMiddleX, AtMiddleY)) [] [line xline, line yline] Nothing)
  //line f = f Nothing (px 10.0) <@< {stroke = toSVGColor "white"} <@< {strokewidth = px 2.0}

//tStartSymb :: Image s
//tStartSymb = polygon Nothing [ (px 0.0, px 0.0), (px 16.0, px 8.0), (px 0.0, px 16.0) ]

//tStopSymb :: Image s
//tStopSymb  = rect (px 16.0) (px 16.0)

//tTaskDef :: String String [(String, String)] (Image s) -> Image s
//tTaskDef taskName resultTy taskArgsAndTys tdbody
  //# bgRect       = rect maxXSpan (imageyspan (imageTag "tTaskDef_taskNameImg") + imageyspan (imageTag "tTaskDef_taskArgsImgs") + imageyspan (imageTag "tTaskDef_taskBodyImgs"))
                     //<@< { fill        = toSVGColor "white" }
                     //<@< { stroke      = toSVGColor "black" }
                     //<@< { strokewidth = px 1.0 }
                     //<@< { xradius     = px 5.0 }
                     //<@< { yradius     = px 5.0 }
  //# taskNameImg  = tag (imageTag "tTaskDef_taskNameImg")  (margin (px 5.0) (text ArialBold10px (taskName +++ " yields an " +++ resultTy))) // TODO a/an
  //# taskArgsImgs = tag (imageTag "tTaskDef_taskArgsImgs") (margin (px 5.0) (above (repeat AtLeft) [] (map (text ArialRegular10px o mkArgAndTy) taskArgsAndTys) Nothing))
  //# taskBodyImgs = tag (imageTag "tTaskDef_taskBodyImgs") (margin (px 5.0) tdbody)
  //# taskContents = above (repeat AtLeft) [] (case taskArgsAndTys of
                                               //[] -> [taskNameImg, xline Nothing maxXSpan, taskBodyImgs]
                                               //_  -> [taskNameImg, xline Nothing maxXSpan, taskArgsImgs, xline Nothing maxXSpan, taskBodyImgs]) Nothing
  //# tTaskDef     = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskContents] Nothing
  //= tTaskDef
  //where
  //maxXSpan = maxSpan [imagexspan (imageTag "tTaskDef_taskNameImg"), imagexspan (imageTag "tTaskDef_taskArgsImgs"), imagexspan (imageTag "tTaskDef_taskBodyImgs")]
  //mkArgAndTy (arg, ty) = arg +++ " is a " +++ ty // TODO a/an

//tTransformApp :: String [String] -> Image s
//tTransformApp tffun args
  //# bgRect     = rect maxXSpan (imageyspan (imageTag "tTransformApp_tfNameImg") + imageyspan (imageTag "tTransformApp_tfArgsImgs"))
                   //<@< { fill        = toSVGColor "white" }
                   //<@< { stroke      = toSVGColor "black" }
                   //<@< { strokewidth = px 1.0 }
  //# tfNameImg  = tag (imageTag "tTransformApp_tfNameImg")  (margin (px 5.0) (text ArialItalic10px tffun))
  //# tfArgsImgs = tag (imageTag "tTransformApp_tfArgsImgs") (margin (px 5.0) (above (repeat AtLeft) [] (map (text ArialItalic10px) args) Nothing))
  //# tfContents = above (repeat AtLeft) [] (case args of
                                             //[] -> [tfNameImg]
                                             //_  -> [tfNameImg, xline Nothing maxXSpan, tfArgsImgs]) Nothing
  //# tfApp      = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, tfContents] Nothing
  //= tfApp
  //where
  //maxXSpan = maxSpan [imagexspan (imageTag "tTransformApp_tfNameImg"), imagexspan (imageTag "tTransformApp_tfArgsImgs")]

//tTaskApp :: String [String] -> Image s
//tTaskApp taskName taskArgs
  //# bgRect       = rect maxXSpan (imageyspan (imageTag "tTaskApp_taTaskNameImg") + imageyspan (imageTag "tTaskApp_taTaskArgsImgs"))
                     //<@< { fill        = toSVGColor "white" }
                     //<@< { stroke      = toSVGColor "black" }
                     //<@< { strokewidth = px 1.0 }
                     //<@< { xradius     = px 5.0 }
                     //<@< { yradius     = px 5.0 }
  //# taskNameImg  = tag (imageTag "tTaskApp_taTaskNameImg")  (margin (px 5.0) (text ArialBold10px taskName))
  //# taskArgsImgs = tag (imageTag "tTaskApp_taTaskArgsImgs") (margin (px 5.0) (above (repeat AtLeft) [] (map (text ArialRegular10px) taskArgs) Nothing))
  //# taskText     = above (repeat AtMiddleX) [] (case taskArgs of
                                                  //[] -> [taskNameImg]
                                                  //_  -> [taskNameImg, xline Nothing maxXSpan, taskArgsImgs]) Nothing
  //# taskApp      = overlay (repeat (AtMiddleX, AtMiddleY)) [] [bgRect, taskText] Nothing
  //= taskApp
  //where
  //maxXSpan = maxSpan [imagexspan (imageTag "tTaskApp_taTaskNameImg"), imagexspan (imageTag "tTaskApp_taTaskArgsImgs")]

//tReturn :: String -> Image s
//tReturn retval
  //# retText = text ArialRegular10px retval
  //# oval    = ellipse (textxspan ArialRegular10px retval) (px 20.0)
                //<@< { fill        = toSVGColor "white" }
                //<@< { stroke      = toSVGColor "black" }
                //<@< { strokewidth = px 1.0 }
  //= overlay (repeat (AtMiddleX, AtMiddleY)) [] [oval, retText] Nothing
