module grids

//	module to demonstrate independency of GridDimension and GridLayout
import iTasks
import iTasks.API.Extensions.SVG.SVGlet

Start :: *World -> *World
Start world = startEngine viewAll world

viewAll :: Task [((), ())]
viewAll = allTasks (map f imgs)
  where
  f :: (Image (), Image ()) -> Task ((), ())
  f (txt, img) = viewInformation "Code" [imageView (const txt)] () -&&- viewInformation "Image" [imageView (const img)] ()

imgs :: [(Image (), Image ())]
imgs = [(lines ["grid " +++ dimstr +++ " (RowMajor,LeftToRight,TopToBottom) [] [] [1.." +++ toString nr_elts +++ "]"
               ], grid dim (RowMajor,LeftToRight,TopToBottom) [] [] imgs host)
       ,(lines ["grid " +++ dimstr +++ " (RowMajor,RightToLeft,TopToBottom) [] [] [1.." +++ toString nr_elts +++ "]"
               ], grid dim (RowMajor,RightToLeft,TopToBottom) [] [] imgs host)
       ,(lines ["grid " +++ dimstr +++ " (ColumnMajor,LeftToRight,TopToBottom) [] [] [1.." +++ toString nr_elts +++ "]"
               ], grid dim (ColumnMajor,LeftToRight,TopToBottom) [] [] imgs host)
       ,(lines ["grid " +++ dimstr +++ " (ColumnMajor,RightToLeft,TopToBottom) [] [] [1.." +++ toString nr_elts +++ "]"
               ], grid dim (ColumnMajor,RightToLeft,TopToBottom) [] [] imgs host)
       
       ,(lines ["grid " +++ dimstr +++ " (RowMajor,LeftToRight,BottomToTop) [] [] [1.." +++ toString nr_elts +++ "]"
               ], grid dim (RowMajor,LeftToRight,BottomToTop) [] [] imgs host)
       ,(lines ["grid " +++ dimstr +++ " (RowMajor,RightToLeft,BottomToTop) [] [] [1.." +++ toString nr_elts +++ "]"
               ], grid dim (RowMajor,RightToLeft,BottomToTop) [] [] imgs host)
       ,(lines ["grid " +++ dimstr +++ " (ColumnMajor,LeftToRight,BottomToTop) [] [] [1.." +++ toString nr_elts +++ "]"
               ], grid dim (ColumnMajor,LeftToRight,BottomToTop) [] [] imgs host)
       ,(lines ["grid " +++ dimstr +++ " (ColumnMajor,RightToLeft,BottomToTop) [] [] [1.." +++ toString nr_elts +++ "]"
               ], grid dim (ColumnMajor,RightToLeft,BottomToTop) [] [] imgs host)
       ]
where
	(dim,dimstr)= (Rows nr_rows, "(Rows " +++ toString nr_rows +++ ")")
	nr_elts		= 19
	nr_rows		= 3
	nr_cols		= nr_elts / nr_rows + 1
	imgs		= [  overlay [(AtMiddleX,AtMiddleY)] [] [text (normalFontDef "Times New Roman" 8.0) (toString i)]
	                         (Just (rect w h <@< {stroke = toSVGColor "black"} <@< {fill = toSVGColor "white"}))
	              \\ i <- [1..nr_elts]
	              ]
	host		= Nothing

w				= px 50.0
h				= px 14.0

lines txt = above (repeat AtLeft) [] (map (text (normalFontDef "Times New Roman" 10.0)) txt) Nothing
