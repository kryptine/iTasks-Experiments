module Auction

import iTasks
import iTasks.API.Extensions.Admin.UserAdmin
import iTasks.API.Extensions.Admin.TonicAdmin
import iTasks._Framework.Tonic

//:%s/<I\d*, E\d*>\|<\d*>//g 

Start :: *World -> *World
Start world = startEngine [ publish "/" (WebApp []) (\_ -> runAuction)
                          , publish "/tonic" (WebApp []) (\_ -> tonicDashboard [])
                          ] world


runAuction :: Task Void
runAuction = forever (
      (viewTitle "English Auction"
  ||- enterInformation ("Login", "Enter your credentials and login") [])
  >>* [ OnAction (Action "Login" [ActionIcon "login", ActionKey (unmodified KEY_ENTER)]) (hasValue authenticatedTonic)
      ])
  where
  authenticatedTonic {Credentials|username, password}
    =            authenticateUser username password >>=
      \mbUser -> case mbUser of
                   Just user -> workAs user (prepAuction user) >>| return Void
                   Nothing   -> viewInformation (Title "Login failed") [] "Your username or password is incorrect" >>| return Void

wares :: Shared [String]
wares = sharedStore "Wares" []

prepAuction cu = get users >>- \users -> auction (updateInformation "Enter a new bid" []) cu users {Bid | user = cu, ware = "Awesome", price = 1337}

:: Bid =
  {  user   :: User
  ,  ware   :: String
  ,  price  :: Int
  }

derive class iTask Bid

//auction :: (Bid -> Task Bid) User [User] Bid -> Task Bid
//auction bidf auctioneer bidders current =
  //let g bidf auctioneer bidders current =
         //anyTask [b @: bidf current \\ b <- [auctioneer : bidders]] >>= \newBid ->
           //case newBid.price > current.price of
             //True  -> (\current -> g bidf auctioneer bidders current) newBid
             //False -> case newBid.user == auctioneer of
                        //True  -> return current
                        //False -> g bidf auctioneer bidders current
  //in g bidf auctioneer bidders current

auction :: (Bid -> Task Bid) User [User] Bid -> Task Bid
auction bidf auctioneer bidders current =
  anyTask [b @: bidf current \\ b <- [auctioneer : bidders]] >>= \newBid ->
    case newBid.price > current.price of
      True  -> (\current -> auction bidf auctioneer bidders current) newBid
      False -> case newBid.user == auctioneer of
                 True  -> return current
                 False -> auction bidf auctioneer bidders current
