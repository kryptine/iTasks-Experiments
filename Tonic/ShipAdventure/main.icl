module main

import iTasks._Framework.Tonic
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks.API.Extensions.SVG.SVGlet
from Data.Func import mapSt
import StdArray
import Data.Data
import Adventure.Logging
import ShipAdventure.Core, ShipAdventure.Types

// main tasks

Start :: *World -> *World
Start world
  = startEngine
      [ publish "/"        (WebApp []) (\_-> importDemoUsersFlow >>| loginAndManageWorkList "Adventure" myTasks)
      , publish "/tonic"   (WebApp []) (\_-> tonicDashboard [])
      , publish "/map"     (WebApp []) (\_-> showMap)
      , publish "/alarm"   (WebApp []) (\_-> setRoomDetectors)
      , publish "/log"     (WebApp []) (\_-> showLog)
      , publish "/devices" (WebApp []) (\_-> manageDevices)
      ] world
