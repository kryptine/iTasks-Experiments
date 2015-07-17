module static_image_tests

import iTasks
import iTasks.API.Extensions.SVG.SVGlet

Start :: *World -> *World
Start world = startEngine (viewInformation "test" [imageView show_it] ()) world
where
	show_it :: () -> Image ()
	//show_it _ = margin (px d,px d,px d,px d) (circular (d * 6.0) imgs)
	/*show_it _ = maskWith (overlay [] [(px (~r),px (~r))] [circle (px (r*2.0)) <@< {fill = toSVGColor "white"}] (Just (rect (px (2.0*r)) (px (2.0*r)))))
	                     (polygon Nothing [(px zero,px zero),(px (2.0 * r * cos a),px (2.0 * r * sin a)),(px (2.0 * r * cos b),px (2.0 * r * sin b))])
//	                     (polygon Nothing [(px r,px zero),(px (2.0 * r),px r),(px zero,px r)])
*/
	show_it _ = beside [] [] [ polygon Nothing [(px (0.0*r),px (1.0*r)), (px (0.5*r),px (-1.0*r)),(px (1.0*r),px (1.0*r))]
	                         , polygon Nothing [(px (0.0*r),px (2.0*r)), (px (0.5*r),px ( 0.0*r)),(px (1.0*r),px (2.0*r))]
	                         , polygon Nothing [(px (1.0*r),px (3.0*r)), (px (1.5*r),px ( 1.0*r)),(px (2.0*r),px (3.0*r))]
	                         , polygon Nothing [(px (0.5*r),px ( 0.0*r)),(px (0.0*r),px (2.0*r)), (px (1.0*r),px (2.0*r))]
	                         ] Nothing
	where
		r		= 100.0
		a		= 0.25 * pi
		b		= 0.75 * pi


pi =: 3.1415926
d  =  50.0

circular :: Real [Image m] -> Image m
circular r imgs				= overlay (repeat (AtMiddleX,AtMiddleY)) 
							          [(px (~r * cos (i*alpha - pi/2.0)),px (~r * sin (i*alpha - pi/2.0))) \\ i <- [0.0, 1.0 ..] & img <- imgs] 
							          [rotate (Rad (i*alpha)) img \\ i <- [0.0, 1.0 ..] & img <- imgs] 
							          (Just (empty (px (2.0*r)) (px (2.0*r))))
where
	n     				    = length imgs
	alpha					= 2.0 * pi / (toReal n)

imgs          = [img_wide,img_high,img_square,img_wwide,img_whigh,img_wsquare]
img_wide      = overlay [(AtMiddleX,AtMiddleY)] [] [text (font (d / 4.0)) "A" <@< {stroke = toSVGColor "white"}] (Just (rect (px (d * 5.0)) (px d)))
img_high      = overlay [(AtMiddleX,AtMiddleY)] [] [text (font d)         "B" <@< {stroke = toSVGColor "white"}] (Just (rect (px d) (px (d * 4.0))))
img_square    = overlay [(AtMiddleX,AtMiddleY)] [] [text (font (d / 4.0)) "C" <@< {stroke = toSVGColor "white"}] (Just (rect (px d) (px d)))
img_wwide     = overlay [(AtMiddleX,AtMiddleY)] [] [text (font (d / 4.0)) "D" <@< {stroke = toSVGColor "black"}] (Just (framed (rect (px (d * 5.0)) (px d))))
img_whigh     = overlay [(AtMiddleX,AtMiddleY)] [] [text (font d)         "E" <@< {stroke = toSVGColor "black"} <@< {fill = toSVGColor "none"}] (Just (framed (rect (px d) (px (d * 4.0)))))
img_wsquare   = overlay [(AtMiddleX,AtMiddleY)] [] [text (font (d / 4.0)) "F" <@< {stroke = toSVGColor "black"}] (Just (framed (rect (px d) (px d))))
framed img    = img <@< {fill = toSVGColor "white"} <@< {stroke = toSVGColor "black"}

font ys       = { fontfamily = "Verdana", fontysize = ys, fontstretch = "normal", fontstyle = "normal", fontvariant = "normal", fontweight = "normal" }
