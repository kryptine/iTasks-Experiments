module SimpleSVG

/* How to create and test an iTask program:
   1. Install the iTask Clean compiler
   2. Create a new project with this module as main module
   3. Select the 'iTasks' environment
   4. Bring the project Up-Uo-Date
   5. Start the generated exe (this launches a local web server, allow your OS to unblock the exe)
   6. Open a web browser (Google Chrome gives the best results)
   7. Navigate to http://localhost/ activates the root-application (hello world)
   8. Navigate to http://localhost/LABEL activates the application with the corresponding LABEL (e.g. http://localhost/basic)
*/

import iTasks								// the iTask API
import iTasks.API.Extensions.SVG.SVGlet		// required to embed Image-tasks inside other tasks
import StdArray

const2 :: .a .b .c -> .a
const2 x _ _ = x

:: Person = {name :: String, surname :: String, birth :: Date}
derive class iTask Person

person = {name = "Peter", surname = "Achten", birth = {day=9,mon=1,year=1967}}

Start :: *World -> *World
Start world
	= startEngine [publish "/"                (WebApp []) (const (viewInformation "Hello" [] "World!"                                                     <<@ FullScreen))
	              ,publish "/time"            (WebApp []) (const (viewSharedInformation "Time"      []                            currentTime             <<@ FullScreen))
	              ,publish "/person"          (WebApp []) (const (updateInformation "Person" [] person >>= \p -> return (p === person)                    <<@ FullScreen))
	              ,publish "/basic"           (WebApp []) (const (viewInformation "Basic Images"    [imageView basic_images       (const2 Nothing)] model <<@ FullScreen))
	              ,publish "/transformations" (WebApp []) (const (viewInformation "Transformations" [imageView transformed_images (const2 Nothing)] model <<@ FullScreen))
	              ,publish "/overlays"        (WebApp []) (const (viewInformation "Overlays"        [imageView overlays           (const2 Nothing)] model <<@ FullScreen))
	              ,publish "/linear"          (WebApp []) (const (viewInformation "Linear"          [imageView linear             (const2 Nothing)] model <<@ FullScreen))
	              ,publish "/grid"            (WebApp []) (const (viewInformation "Grid"            [imageView grid_layouts       (const2 Nothing)] model <<@ FullScreen))
	              ,publish "/box"             (WebApp []) (const (viewInformation "Box"             [imageView box2               (const2 Nothing)] model <<@ FullScreen))
	              ,publish "/rose"            (WebApp []) (const (viewInformation "Rose"            [imageView rose               (const2 Nothing)] model <<@ FullScreen))
	              ,publish "/onclick"         (WebApp []) (const (updateInformation "On-Click"    [imageUpdate id count (\_ _ -> Nothing) (\_ n -> n) ] 0 <<@ FullScreen))
	              ,publish "/100percent"      (WebApp []) (const (viewInformation "100% Clean!"     [imageView clean              (const2 Nothing)] model <<@ FullScreen))
	              ] world
where
	model = ()								// for these examples, the model is actually irrelevant

//	This examples displays the basic Image shapes
basic_images :: m *TagSource -> Image m
basic_images model tags
	= margin (px zero,px 100.0,px zero,px zero) (
	    grid (Columns 3) (RowMajor,LeftToRight,TopToBottom) (updateAt 6 (AtLeft,AtMiddleY) (repeat (AtLeft,AtTop))) []
	       [ above [] [] [empty (px 200.0) (px 100.0),    txts ["empty (px 200.0) (px 100.0)"]] Nothing
	       , above [] [] [margin (px zero,px 5.0,px zero,px 5.0) (rect  (px 200.0) (px 100.0))
	                                                    , txts ["rect (px 200.0) (px 100.0)"]] Nothing
	       , above [] [] [rect  (px 200.0) (px 100.0) <@< {fill = toSVGColor "none"}
	                                                    , txts ["rect (px 200.0) (px 100.0)"
	                                                           ,"<@< {fill = toSVGColor \"none\"}"
	                                                           ]] Nothing
	       , above [] [] [circle  (px 100.0),             txts ["circle (px 100.0)"]] Nothing
	       , above [] [] [ellipse (px 200.0) (px 100.0),  txts ["ellipse (px 200.0) (px 100.0)"]] Nothing
	       , above [] [] [overlay [] [] 
	                         [text (normalFontDef "Times New Roman" 100.0) "Hey World!"]
	                         (Just (empty (px 200.0) (px 100.0)))
	                                                    , txts ["text (normalFontDef \"Times New Roman\" 100.0) \"Hey World!\""]] Nothing
	       , above [] [] [xline Nothing (px 200.0),       txts ["xline Nothing (px 200.0)"]] Nothing
	       , above [AtMiddleX] [] [yline Nothing (px 100.0),       txts ["yline Nothing (px 100.0)"]] Nothing
	       , above [] [] [line Nothing Slash (px 200.0) (px 100.0)
	                                                    , txts ["line Nothing Slash (px 200.0) (px 100.0)"]] Nothing
	       , above [] [] [line Nothing Backslash (px 200.0) (px 100.0)
	                                                    , txts ["line Nothing Backslash (px 200.0) (px 100.0)"]] Nothing
	       , above [] [] [polygon  Nothing [(zero,zero),(px 200.0,px 100.0),(px 200.0,zero),(zero,px 100.0)]
	                                                    , txts ["polygon Nothing"
	                                                           ,"        [(zero,    zero    )"
	                                                           ,"        ,(px 200.0,px 100.0)"
	                                                           ,"        ,(px 200.0,zero    )"
	                                                           ,"        ,(zero,    px 100.0)]"]] Nothing
	       , above [] [] [polyline Nothing [(zero,zero),(px 200.0,px 100.0),(px 200.0,zero),(zero,px 100.0)]
	                                                    , txts ["polyline Nothing"
	                                                           ,"         [(zero,    zero    )"
	                                                           ,"         ,(px 200.0,px 100.0)"
	                                                           ,"         ,(px 200.0,zero    )"
	                                                           ,"         ,(zero,    px 100.0)]"]] Nothing
	       ] Nothing
	  )
where
	txts lines = margin (px 5.0,px 10.0,px 10.0,px 10.0) (above [] [] (map (text (normalFontDef "Lucida Console"  10.0)) lines) Nothing)

//  This examples shows all possible transformations on (composite) Image-s:
transformed_images :: m *TagSource -> Image m
transformed_images model tags
	= margin (px 100.0) (
	    grid (Columns 4) (RowMajor,LeftToRight,TopToBottom) (repeat (AtMiddleX,AtBottom)) []
	       [ above (repeat AtMiddleX) [] [img,                    txt "img"]                           Nothing
	       , above (repeat AtMiddleX) [] [fit    (px  100.0) 
	                                             (px  100.0) img, txt "fit (px 100.0) (px 100.0) img"] Nothing
	       , above (repeat AtMiddleX) [] [fitx   (px  100.0) img, txt "fitx (px 100.0) img"]           Nothing
	       , above (repeat AtMiddleX) [] [fity   (px  100.0) img, txt "fity (px 100.0) img"]           Nothing
	       , above (repeat AtMiddleX) [] [rotate (deg -20.0) img, txt "rotate (deg -20.0) img "]       Nothing
	       , above (repeat AtMiddleX) [] [rotate (deg  20.0) img, txt " rotate (deg 20.0) img"]        Nothing
	       , above (repeat AtMiddleX) [] [skewx  (deg -20.0) img, txt "skewx (deg -20.0) img "]        Nothing
	       , above (repeat AtMiddleX) [] [skewx  (deg  20.0) img, txt " skewx (deg 20.0) img"]         Nothing
	       , above (repeat AtMiddleX) [] [flipx              img, txt "flipx img"]                     Nothing
	       , above (repeat AtMiddleX) [] [flipy              img, txt "flipy img"]                     Nothing
	       , above (repeat AtMiddleX) [] [skewy (deg -20.0)  img, txt "skewy (deg -20.0) img"]         Nothing
	       , above (repeat AtMiddleX) [] [skewy (deg  20.0)  img, txt "skewy (deg 20.0) img"]          Nothing
	       ] Nothing
	  )
where
	img   = text (normalFontDef "Times New Roman" 50.0) "F"
	txt s = text (normalFontDef "Lucida Console"  10.0) s

//	This example shows all overlay-combinations:
overlays :: m *TagSource -> Image m
overlays model tags
	= margin (px 10.0) (
	     grid (Rows 3) (RowMajor,LeftToRight,TopToBottom) [] []
	        [ beside (repeat AtMiddleY) [] 
	                 [ margin (px 5.0) (overlay (repeat (x_align,y_align)) [] discs Nothing)
	                 , txt ("(" <+++ x_align <+++ "," <+++ y_align <+++ ")*")
	                 ] Nothing
	        \\ x_align <- [AtLeft,AtMiddleX,AtRight]
	         , y_align <- [AtTop, AtMiddleY,AtBottom]
	        ] Nothing
	  )
where
	txt s = text (normalFontDef "Lucida Console"  10.0) s

//	This example shows all beside and above combinations:
linear :: m *TagSource -> Image m
linear model tags
	= margin (px 10.0) (
	     beside (repeat AtTop) []
	         [ beside (repeat AtMiddleY) []
	              [ txt "  beside  " <@< {stroke = toSVGColor "blue"} <@< {fill = toSVGColor "blue"}
	              , above (repeat AtLeft) []
	                  [ beside (repeat AtMiddleY) [] [ beside (repeat y_align) [] discs Nothing
	                                                 , txt ("  " <+++ y_align <+++ "*")
	                                                 ] Nothing
	                  \\ y_align <- [AtTop,AtMiddleY,AtBottom]
	                  ] Nothing
	              ] Nothing
	         , beside (repeat AtMiddleY) []
	              [ txt "  above  " <@< {stroke = toSVGColor "blue"} <@< {fill = toSVGColor "blue"}
	              , beside (repeat AtTop) []
	                  [ above (repeat AtMiddleX) [] [ txt ("  " <+++ x_align <+++ "*")
	                                                , above (repeat x_align) [] discs Nothing
	                                                ] Nothing
	                  \\ x_align <- [AtLeft,AtMiddleX,AtRight]
	                  ] Nothing
	              ] Nothing
	         ] Nothing
	  )
where
	txt s = text (normalFontDef "Lucida Console"  10.0) s

//	This example shows all grid-layout combinations:
grid_layouts :: m *TagSource -> Image m
grid_layouts model tags
	= margin (px zero) (
	     grid (Columns 4) (RowMajor,LeftToRight,TopToBottom) [] []
	        [  above (repeat AtMiddleX) []
	              [ margin (px 5.0,px zero) (grid (Columns 2) (major,x_fill,y_fill) [] [] discs Nothing)
	              , txt (" (" <+++ major <+++ "," <+++ x_fill <+++ "," <+++ y_fill <+++ ") ")
	              ] Nothing
	        \\ major  <- [ColumnMajor,RowMajor   ]
	         , x_fill <- [LeftToRight,RightToLeft]
	         , y_fill <- [TopToBottom,BottomToTop]
	        ] Nothing
	  )
where
	txt s = text (normalFontDef "Lucida Console"  10.0) s

//	This example shows the use of ImageTag to display two images inside a rectangle that depends on each others dimensions:
box2 :: m *TagSource -> Image m
box2 _ tags = pair (arrow, rotate (deg -90.0) arrow) tags
where
	arrow = polygon Nothing [(px zero,px -10.0),(px 55.0,px -10.0),(px 50.0,px -30.0),(px 85.0,px zero)
	                        ,(px 50.0,px  30.0),(px 55.0,px  10.0),(px zero,px  10.0)
	                        ]

//	This example shows the use of ImageTag to display an arbitrary rose tree structure:
rose :: m *TagSource -> Image m
rose _ tags = fst (show show_my_node my_rose_tree tags)
where
	show_my_node txt ts
		= (margin (px zero,px zero,px bottom,px zero) (
		      overlay [(AtMiddleX,AtMiddleY)] []
		          [text font txt]
		          (Just (rect (textxspan font txt + textxspan font "MM") (px (height + text_y_margin)) <@< {fill = toSVGColor "white"})))
		  , ts
		  )
	where
		font          = normalFontDef "Arial" height
		height        = 10.0
		text_y_margin = 5.0
		bottom        = 5.0

//	This examples displays the number of times that you've clicked on the text
count :: Int *TagSource -> Image Int
count n _
	= margin (px zero) (
	    overlay [(AtMiddleX,AtMiddleY)] [] 
	       [ text font (toString n) <@< {fill = toSVGColor "white"}]
	       (Just (rect (textxspan font ("  " <+++ n)) (px (h + m))))
	       <@< {onclick = (+), local = False}
	  )
where
	font = normalFontDef "Times New Roman" h
	h    = 100.0
	m    = 6.0

//	This example shows an image displayed by Marc Schoolderman during 'practicum' friday afternoon, may 22 2015
clean :: m *TagSource -> Image m
clean model tags
	= overlay (repeat (AtMiddleX,AtMiddleY)) []
	     [ star 31 (r_in,r_out)
	     , circle (px r_in *. 1.6) <@< {strokewidth = px bandwidth} <@< {stroke = toSVGColor "white"}
	     , rotate (rad (pi * 0.25)) (circular (px r_in *. 0.8) (2.0 * pi) (repeatn 4 (circle (px bandwidth *. 0.8))))
	     , rotate (rad (pi * 0.32)) (circular (px zero)        (2.0 * pi) (map (arctext (px r_in *. 0.78) (0.4 * pi) narrowfont) ["NO VIRUSES","NO SPYWARE","NO VIRUSES","NO SPYWARE"]))
	     , above (repeat AtMiddleX) [] (map (((>@>) {fill = toSVGColor "white"}) o ((>@>) {stroke = toSVGColor "white"}) o (text bigfont)) ["100%", "CLEAN"]) Nothing
	     ] Nothing
where
	r_out      = 100.0
	r_in       = 90.0
	bandwidth  = r_in * 0.2
	bigfont    = {normalFontDef "Arial"        (r_in * 0.35) & fontweight = "bolder"}
	narrowfont =  normalFontDef "Arial Narrow" (r_in * 0.22)

star :: Int (Real,Real) -> Image m
star n (r_in,r_out)
	= polygon Nothing (flatten 
	     [  [(px r_out *. (cos (angle * (toReal outer_corner))), px r_out *. (sin (angle * (toReal outer_corner))))
	        ,(px r_in  *. (cos (angle * (toReal inner_corner))), px r_in  *. (sin (angle * (toReal inner_corner))))
	        ]
	     \\ outer_corner <- [0, 2 .. 2*n], let inner_corner = outer_corner+1
	     ])
where
	angle = pi / (toReal n)

arctext :: Span Real FontDef String -> Image m
arctext r a font txt
	= circular r a [rotate (rad pi) (text font (toString c)) \\ c <-: txt]

pair :: (Image m,Image m) *TagSource -> Image m
pair (img1,img2) [(t1,ut1),(t2,ut2):tags]
	= beside [] []
	      [overlay [(AtMiddleX,AtMiddleY)] [] [tag ut1 img1] host
	      ,overlay [(AtMiddleX,AtMiddleY)] [] [tag ut2 img2] host
	      ] Nothing
where
	(w1,h1) = (imagexspan t1,imageyspan t1)
	(w2,h2) = (imagexspan t2,imageyspan t2)
	host    = Just (rect (maxSpan [w1,w2]) (maxSpan [h1,h2]) <@< {fill = toSVGColor "none"})

show :: (a -> St *TagSource (Image m)) (Rose a) -> St *TagSource (Image m)
show show_node (Rose r []) 
	= show_node r
show show_node (Rose r rs) 
	= \[(t1,ut1), (t2,ut2) : ts] ->
		let (image,  ts1) = show_node r ts
		    (images, ts2) = seqList (map (show show_node) rs) ts1
		 in ( above (repeat AtLeft) []
		          [ image
		          , beside (repeat AtTop) []
		                   [ yline Nothing (imageyspan t1 - imageyspan t2)
		                   , tag ut1 
		                        (grid (Columns 2) (ColumnMajor,LeftToRight,TopToBottom) [] []
		                              (repeatn (length rs) (xline Nothing (px 10.0)) ++ init images ++ [tag ut2 (last images)])
		                              Nothing
		                        ) 
		                   ] Nothing 
		          ] Nothing
		    , ts2
		    )


discs :: [Image m]
discs = [circle (px 15.0 + px 8.0 *. d) <@< {fill = toSVGColor {r=255-d*25,g=210-d*70,b=210-d*70}} \\ d <- [3,2,1,0]]

derive gText XAlign, YAlign, GridMajor, GridXLayout, GridYLayout

:: Rose a = Rose a [Rose a]

from StdFunc import const, seqList, :: St(..)

my_rose_tree :: Rose String
my_rose_tree = Rose "Clean 2.2 Language Report" 
                  [Rose "BASIC SEMANTICS" 
                      [Rose "Graph Rewriting" 
                          [Rose "A Small Example" []]
                      ,Rose "Global Graphs" []
                      ]
                  ,Rose "MODULES AND SCOPES"
                      [Rose "Identifiers, Scopes and Name Spaces" 
                          [Rose "Naming Conventions of Identifiers" []
                          ,Rose "Scopes and Name Spaces" []
                          ,Rose "Nesting of Scopes" []
                          ]
                      ,Rose "Modular Structure of Clean Programs" []
                      ,Rose "Implementation Modules" 
                          [Rose "The Main or Start Module" []
                          ,Rose "Scope of Global Definitions in Implementation Modules" []
                          ,Rose "Begin and End of a Definition: the Layout Rule" []
                          ]
                      ,Rose "Definition Modules" []
                      ,Rose "Importing Definitions" 
                          [Rose "Explicit Imports of Definitions" []
                          ,Rose "Implicit Imports of Definitions" []
                          ]
                      ,Rose "System Definition and Implementation Modules" []
                      ]
                  ,Rose "DEFINING FUNCTIONS AND CONSTANTS"
                      [Rose "Functions" []
                      ,Rose "Patterns" []
                      ,Rose "Guards" []
                      ,Rose "Expressions"
                          [Rose "Lambda Abstraction" []
                          ,Rose "Case Expression and Conditional Expression" []
                          ]
                      ,Rose "Local Definitions" 
                          [Rose "Let Expression: Local Definitions in Expressions" []
                          ,Rose "Where Block: Local Definitions in a Function Alternative" []
                          ,Rose "With Block: Local Definitions in a Guarded Alternative" []
                          ,Rose "Let-Before Expression: Local Constants defined between Guards" []
                          ]
                      ,Rose "Defining Constants" 
                          [Rose "Selectors" []]
                      ,Rose "Typing Functions" 
                          [Rose "Typing Curried Functions" []
                          ,Rose "Typing Operators" []
                          ,Rose "Typing Partial Functions" []
                          ,Rose "Explicit use of the Universal Quantifier in Function Types" []
                          ,Rose "Functions with Strict Arguments" []
                          ]
                      ]
                  ,Rose "PREDEFINED TYPES"
                      [Rose "Basic Types: Int, Real, Char and Bool" 
                          [Rose "Creating Constant Values of Basic Types" []
                          ,Rose "Patterns of Basic Types" []
                          ]
                      ,Rose "Lists" 
                          [Rose "Creating Lists" []
                          ,Rose "List Patterns" []
                          ]
                      ,Rose "Tuples" 
                          [Rose "Creating Tuples" []
                          ,Rose "Tuple Patterns" []
                          ]
                      ,Rose "Arrays" 
                          [Rose "Creating Arrays and Selection of field Elements" []
                          ,Rose "Array Patterns" []
                          ]
                      ,Rose "Predefined Type Constructors" []
                      ,Rose "Arrow Types" []
                      ,Rose "Predefined Abstract Types" []
                      ]
                  ,Rose "DEFINING NEW TYPES"
                      [Rose "Defining Algebraic Data Types" 
                          [Rose "Using Constructors in Patterns" []
                          ,Rose "Using Higher Order Types" []
                          ,Rose "Defining Algebraic Data Types with Existentially Quantified Variables" []
                          ,Rose "Defining Algebraic Data Types with Universally Quantified Variables" []
                          ,Rose "Strictness Annotations in Type Definitions" []
                          ,Rose "Semantic Restrictions on Algebraic Data Types" []
                          ]
                      ,Rose "Defining Record Types"
                          [Rose "Creating Records and Selection of Record Fields" []
                          ,Rose "Record Patterns" []
                          ]
                      ,Rose "Defining Synomym Types" []
                      ,Rose "Defining Abstract Data Types" 
                          [Rose "Defining Abstract Data Types with Synonym Type Definition" []]
                      ]
                  ,Rose "OVERLOADING"
                      [Rose "Type Classes" []
                      ,Rose "Functions Defined in Terms of Overloaded Functions" []
                      ,Rose "Instances of Type Classes Defined in Terms of Overloaded Functions" []
                      ,Rose "Type Constructor Classes" []
                      ,Rose "Overlapping Instances" []
                      ,Rose "Internal Overloading" []
                      ,Rose "Defining Derived Members in a Class" []
                      ,Rose "A Shorthand for Defining Overloaded Functions" []
                      ,Rose "Classes Defined in Terms of Other Classes" []
                      ,Rose "Exporting Type Classes" []
                      ,Rose "Semantic Restrictions on Type Classes" []
                      ]
                  ,Rose "GENERIC PROGRAMMING"
                      [Rose "Basic Ideas Behing Generic Programming" []
                      ,Rose "Defining Generic Functions" []
                      ,Rose "Deriving Generic Functions" []
                      ,Rose "Applying Generic Functions" []
                      ,Rose "Using Constructor Information" []
                      ,Rose "Generic Functions and Uniqueness Typing" []
                      ,Rose "Exporting Generic Functions" []
                      ]
                  ,Rose "DYNAMICS"
                      [Rose "Packing Expressions into a Dynamic" 
                          [Rose "Packing Abstract Data Types" []
                          ,Rose "Packing Overloaded Functions" []
                          ,Rose "Packing Expressions of Unique Type" []
                          ,Rose "Packing Arguments of Unknown Type" []
                          ,Rose "Using Dynamic Typing to Defeat the Static Type System" []
                          ]
                      ,Rose "Unpacking Dynamics Using a Dynamic Pattern Match" 
                          [Rose "Unpacking Abstract Data Types" []
                          ,Rose "Unpacking of Overloaded Functions" []
                          ,Rose "Unpacking Expressions of Unique Type" []
                          ,Rose "Checking and Unifying Types Schemes using Type Pattern Variables" []
                          ,Rose "Checking and Unifying Unknown Types using Overloaded Type Variables" []
                          ]
                      ,Rose "Type Safe Communication using Dynamics" []
                      ,Rose "Architecture of the implementation" []
                      ,Rose "Semantic Restrictions on Dynamics" []
                      ]
                  ,Rose "UNIQUENESS TYPING"
                      [Rose "Basic Ideas behind Uniqueness Typing" []
                      ,Rose "Attribute Propagation" []
                      ,Rose "Defining New Types with Uniqueness Attributes" []
                      ,Rose "Uniqueness and Sharing" 
                          [Rose "Higher Order Uniqueness Typing" []
                          ,Rose "Uniqueness Type Coercions" []
                          ]
                      ,Rose "Combining Uniqueness Typing and Overloading" 
                          [Rose "Constructor Classes" []]
                      ,Rose "Higher-Order Type Definitions" []
                      ,Rose "Destructive Updates using Uniqueness Typing" []
                      ]
                  ,Rose "STRICTNESS, MACROS AND EFFICIENCY"
                      [Rose "Annotations to Change Lazy Evaluation into Strict Evaluation" 
                          [Rose "Advantages and Disadvantages of Lazy versus Strict Evaluation" []
                          ,Rose "Strict and Lazy Context" []
                          ,Rose "Space Consumption in Strict and Lazy Context" []
                          ,Rose "Time Consumption in Strict and Lazy Context" []
                          ,Rose "Changing Lazy into Strict Evaluation" []
                          ]
                      ,Rose "Defining Graphs on the Global Level" []
                      ,Rose "Defining Macros" []
                      ,Rose "Efficiency Tips" []
                      ]
                  ,Rose "FOREIGN LANGUAGE INTERFACE"
                      [Rose "Foreign Export" []
                      ,Rose "Using ABC instructions" []
                      ]
                  ]


//	a generally useful image combinator:
circular :: !Span !Real ![Image m] -> Image m
circular r a imgs
  #! n      = length imgs
  #! sign_a = toReal (sign a)
  #! a`     = normalize (rad a)
  #! alpha  = (toRad a`) / (toReal n)
  = overlay (repeat (AtMiddleX,AtMiddleY))
                    [(~r *. cos angle,~r *. sin angle) \\ i <- [0.0, sign_a ..], angle <- [i*alpha - 0.5*pi]]
                    [rotate (rad (i*alpha)) img \\ i <- [0.0, sign_a ..] & img <- imgs]
                    (Just (empty (r *. 2) (r *. 2)))              // BUG: using Nothing creates incorrect image (offset to left)

pi =: 3.14159265359
