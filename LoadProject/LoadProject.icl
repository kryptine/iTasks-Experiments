module LoadProject

import PmProject
import Text
import iTasks.API.Extensions.CodeMirror
import Data.Maybe
import System.File
import UtilNewlinesFile
import StdMisc

genCodeMirror :: [String] Int -> CodeMirror
genCodeMirror sourceCode line

	  = { configuration = [CMMode "haskell", CMLineNumbers True, CMReadOnly False, CMStyleActiveLine True]
        , position = (line-1,0)
        , selection = Nothing
        , highlighted = [((line-1,0),(line,0))]
        , source = sourceCode 
		}
		
getModulePath :: Project String -> String
getModulePath prj moduleName 
	= maybe "" (\mi -> mi.dir </> moduleName +++ ".icl") (PR_GetModuleInfo moduleName prj)


readLinesTask :: String -> Task [String]
readLinesTask path 
	= accWorld (readLines path)
where
	readLines :: String !*World -> ([String], !*World)
	readLines path world
		# (ok, f, world) = fopen path FReadText world
		| not ok
			= abort "File not found"
		# (_, lines, f) = readConvLines f
		# (_, world) = fclose f world
		= (lines, world)


:: ModuleLine = 
	{ moduleName :: String
	, lineNumber :: Int
	}
	
derive class iTask ModuleLine, FileError

showModule :: Project -> Task CodeMirror
showModule prj = enterInformation "Module and Line" [] >>= \ml ->
	viewInformation "Path" [] (getModulePath prj ml.moduleName) >>|
	readLinesTask (getModulePath prj ml.moduleName)  >>= \lines ->
	(updateInformation "Buu" [UpdateWith (\cm -> codeMirrorEditlet cm []) (\_ editlet -> editlet.currVal)] (genCodeMirror lines ml.lineNumber))
		//<<@ AfterLayout (tweakUI (setSize (ExactSize 600) (ExactSize 600)))

Start :: *World -> *World
Start world 
	# ((prj, success, errormsg), world) = accFiles (ReadProjectFile ".\\LoadProject.prj" "") world
	= startEngine (showModule prj) world 

