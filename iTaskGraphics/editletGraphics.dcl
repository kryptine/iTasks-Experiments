definition module editletGraphics

import iTasks
import iTasks.API.Core.Client.Editlet

svg_image :: Task MR
::  MR = MR [ModelImage] Int

derive JSONEncode     MR
derive JSONDecode     MR
derive gEditMeta      MR
derive gVerify        MR
derive gEq            MR
derive gDefault       MR
derive gText          MR
derive gUpdate        MR
derive gEditor        MR

::  ModelImage  = Line Int ModelLine | Rect Int ModelRect | Circle Int ModelCircle
::  ModelLine   = { pos :: !(!Int,!Int), end :: !(!Int,!Int), linew :: !Int, edge :: !String }
::  ModelRect   = { pos :: !(!Int,!Int), size :: !(!Int,!Int), frame :: !String, framew :: !Int, fill :: !String, opacity :: !Real }
::  ModelCircle = { pos :: !(!Int,!Int), r :: !Int, frame :: !String, framew :: !Int, fill :: !String, opacity :: !Real }
derive class iTask ModelImage, ModelLine, ModelRect, ModelCircle
