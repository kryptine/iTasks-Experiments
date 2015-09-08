module IFL15

import MultiUser
import iTasks
import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin

from Text.Parsers.Parsers import :: Parser
import qualified Text.Parsers.Parsers as PS

import StdArray

Start :: *World -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_-> ifl15 (viewInformation "You have entered" []))
                          , publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
                          ] world

ifl15 :: (Int -> Task Int) -> Task String
ifl15 viewTask
  =           enterInformation "Enter a number" []
  >>= \num -> viewTask num
  >>|         if (num == 42)
                (viewInformation "Awesome!" [] "42!")
                (viewInformation "Less awesome" [] (toString num +++ " is undoubtely a beautiful number too"))

//import System.IO
//main :: IO Int
//main = giveOne >>= \x -> giveTwo >>= \y -> return (x + y)

//giveOne :: IO Int
//giveOne = return 1

//giveTwo :: IO Int
//giveTwo = return 2

//pSymbol x = 'PS'.symbol x

//:: Expr
  //= LetDef Expr Expr Expr
  //| Var String

//derive class iTask Expr

//pExpr :: Parser String t Expr
//pExpr = pVar

//pVar :: Parser String t Expr
//pVar  = return (Var "x")

//// TODO FIXME >>= \_ -> is translated to >>= \_x -> in the compiler. Is messy in blueprints.
//pLetDef :: Parser String t Expr
//pLetDef
  //=          pSymbol "let"
  //>>= \_ ->  pVar
  //>>= \x ->  pSymbol "="
  //>>= \_ ->  pExpr
  //>>= \e ->  pSymbol "in"
  //>>= \_ ->  pExpr
  //>>= \e` -> return (LetDef x e e`)

//chainl1 = chainl1
//pExpr   = chainl1 pTerm   pAddOp
//pTerm   = chainl1 pFactor pMulOp
//pFactor = 'PS'.alternative pDigit ('PS'.symbol "(" >>= \_ -> pExpr >>= \n -> 'PS'.symbol ")" >>= \_ -> return n)
//pDigit  = token (sat isDigit) >>= \x -> return (ord x - ord '0')
//pAddOp  = 'PS'.alternative ('PS'.symbol "+" >>= \_ -> return (+)) ('PS'.symbol "-" >>= \_ -> return (-))
//pMulOp  = 'PS'.alternative ('PS'.symbol "*" >>= \_ -> return (*)) ('PS'.symbol "/" >>= \_ -> return (div))

mySum :: Task Int
mySum =     enterNumber
  >>= \n -> enterNumber
  >>= \m -> return (n + m)

enterNumber :: Task Int
enterNumber = enterInformation "Enter a number" []
