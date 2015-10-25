module BPMC

import iTasks
import MultiUser
import  iTasks.API.Extensions.Email 
import iTasks.Framework.Tonic
import Graphics.Scalable
from Math.Random import genRandInt
import iTasks.API.Extensions.Admin.TonicAdmin

// tasks are assigned to workers with a certain "role", any user administrated with has such role can work on it
// here the follwoing roles are used:

frontOfficer			= UserWithRole "front-office"		// someone working in the front office
csaOfficer				= UserWithRole "csa-office"	 		// someone working in the csa office
midOfficer 				= UserWithRole "mid-office"	 		// someone working in the mid office

// some fake email address

foEmail					= "info@ing.com"					// fake "email" address a client can use to ask for a service

// we assume that for every client we have administrated the following:

:: Client 			  = { name 			:: Name
						, clientNo		:: ClientNo
						, email			:: EmailAddr
					    , accounts  	:: [Account]
					    , login			:: Login
					    , password		:: Passwrd
					    }
:: Name 			  :== String
:: ClientNo			  :== Int
:: Account			  :== Int
:: Login			  :== String
:: Passwrd			  :== String
:: EmailAddr		  :== String	

derive class iTask  Client
	
// lets create a simple database for administrating a list of Client's, with some initial clients

clientDatabase :: Shared [Client]
clientDatabase = sharedStore "ClientDatabase" initialClients
where
	initialClients	=	[ {name = "Rinus" , clientNo = 1, email = "rinus@cs.ru.nl",  login = "rinus",  password = "rinus",  accounts = [1,2]}
			 			, {name = "Pieter", clientNo = 2, email = "pieter@cs.ru.nl", login = "pieter", password = "pieter", accounts = [3..5]}
			 			]

// some handy access functions on this database

getClient :: ClientNo -> Task (Maybe Client)		// find a particular client given a client id ...
getClient cid 	
	=					get clientDatabase
	>>= \clients -> 	let found = searchFor clients in
						return (if (isEmpty found) Nothing (Just (hd found)))
where
	searchFor clients = [client \\ client=:{Client|clientNo} <- clients | cid === clientNo] 

updatePassword :: ClientNo String -> Task ()		// change a password of a client with given client id 
updatePassword cid newPassword
	=					upd setPassword clientDatabase 
	>>|					return ()
where
	setPassword :: 	[Client] -> [Client]
	setPassword clients = [{Client|client & password = if (clientNo==cid) newPassword password} \\ client=:{Client|clientNo,password}<- clients]		



addAccount :: ClientNo String -> Task ()			// add an account to client with given client id 
addAccount cid newAccount
	=					upd add clientDatabase 
	>>|					return ()
where
	add :: 	[Client] -> [Client]
	add clients = [{Client|client & accounts = if (clientNo==cid) [(toInt newAccount):accounts] accounts} \\ client=:{Client|clientNo,accounts}<- clients]		


// The following types are used to generate user interfaces for filling in forms

:: ClientRequest									// Information to fill in by Client asking for a service  	
			= 	{ name			:: Name
			   	, id			:: ClientNo
			   	, account		:: Account
		      	, email			:: EmailAddr
		      	, phone			:: PhoneNumber
		      	, question		:: Note				// textual description of the desired service
		      	}
:: Log a											// Additional logging information which is added to any request
			=	{ about			:: String
				, received_from	:: Name
				, intended_for	:: Name
				, date			:: Date
				, content		:: a
				}
:: Procedure										// what kind of internal procedure do we have ? A made up some...
			= 	Request Service						// request for a particular service
			|	Complaint Note						// a complaint
			|   Other Note							// any thing else which needs some reaction
			|	Stop								// any thing else which cannot be handled
			
:: Service											// Kind of service being asked, I made up some
			=  NewPassword 							// new password
			|  NewAccount							// new account
:: Case												// Case numbers are used to identify stored information
			=	{ caseNo		:: Int
				}

derive class iTask ClientRequest, Log, Procedure, Service, Case

// we need a little database to store information, case numers are used for identification

requestDatabase :: Shared [(Case, (Log ClientRequest, Procedure))]
requestDatabase = sharedStore "caseDatabase" []

messageDatabase :: Shared [(Case,Log Note)]
messageDatabase = sharedStore "messageDatabase" []

// Options one can choose for a given situation

class getOptions a b :: a -> [b] 					// gives a worker a list of values to choose from, for a every kind of request that can be made

instance getOptions Procedure String
where	getOptions (Request NewPassword) = ["abcd12","efgh34","ijkl56","mnop78","qrst90"]
		getOptions (Request NewAccount)  = ["ING 0123456000","ING 0123456001","ING 0123456002","ING 0123456003"]
		getOptions (Complaint (Note s))  = ["We agree","We don't agree","We will contact you"]
		getOptions (Other (Note s))		 = ["We agree","We don't agree","We will contact you"]

class makeModification a :: a ClientNo String -> Task()

instance makeModification Procedure
where	makeModification (Request NewPassword) cid password  = updatePassword cid password 	// update password client
		makeModification (Request NewAccount)  cid account   = updatePassword cid ""
		makeModification (Complaint (Note s))  cid reaction  = return ()					// no action 					
		makeModification (Other (Note s))	   cid reaction  = return ()					// no action 

// Initializing the iTask system ...

Start :: *World -> *World
Start world = StartMultiUserTasks 	[ workflow "case a" "simulation of use case a" caseA	// case a prototype
                                    , workflow "add roles" "add all roles to root" addRoles
									]
                                    [ publish "/tonic" (WebApp []) (\_-> tonicDashboard [])
                                    ] world


addRoles = upd (map addRoles`) userAccounts
  where
  addRoles` acc=:{UserAccount | roles} = { UserAccount | acc & roles = ["front-office" : "csa-office" : "mid-office" : roles]}

// ********************************************************************************************************************************************

// here finally the task description starts...


// task performed by some client, instead of a phone call we assume the client fills in a form of type ClientRequest

caseA :: Task ()																			// step 0															
caseA 
	= 				    get currentUser 													// who is the client logged in ?
	 >>= \client	->	create client frontOfficer "Submit Service Request"	defaultValue	// client fills in service request form 
	 >>= \request  	->	appendTopLevelTaskFor "root" True (handleClientRequest request)		// start the task "handleClientRequest" which will handle the request 
	 >>|				return ()															// done	 

// This is the main workflow task which will handle any requests from a client by assigning tasks to workers

handleClientRequest :: (Log ClientRequest) -> Task () 							
handleClientRequest request
	=					(frontOfficer,"determine request") @: determineProcedure request						// front office has to investigate determine which procedure to follow										
	>>= \procedure ->	case procedure of
							Stop	  -> return () 																// request cannot be handled, done
							otherwise -> handleProcedure (request, procedure)									// continue with handleProcedure
			
handleProcedure :: (Log ClientRequest, Procedure) -> Task ()
handleProcedure (request,procedure)
	= 					(csaOfficer,prompt "log request") @: logServiceRequest (request,procedure) requestDatabase defaultValue	// csa officer steps  6-7
	>>= \caseNo ->		(midOfficer,prompt "diagnose")    @: diagnoseCase (caseNo,(request,procedure))			// mid officer steps 7-8 
	>>= \simple	->	 	if simple (handleRestProcedure (caseNo,(request,procedure)))							// do step 9 and further
								  (return ())																	// non-simple case not specified, it stops here	
where
	prompt s = s <+++ ": " <+++ procedure

handleRestProcedure :: (Case, (Log ClientRequest, Procedure)) -> Task ()
handleRestProcedure serviceCase=:(caseNo,(request,procedure))
	=					(frontOfficer,prompt "resolve") @: resolveRequest procedure 							// front officer steps 9-10,  select solution from option list 				
	>>= \solution ->	(midOfficer,  prompt "modify") 	@: modifyAccount  procedure	 id solution				// mid   officer steps 11-12, user account is updated accordingly
	>>|					(frontOfficer,prompt "done") 	@: inform foEmail email (prompt "Re:") (question,solution) 		// front officer step 13, inform client what has been done
	>>= \mail		 -> (csaOfficer,  prompt "log finished") @: logServiceRequest mail messageDatabase caseNo	// csa   officer step 14, store 
	>>|					return ()																				// stop
where
	prompt s = s <+++ ": " <+++ procedure
	content			   		 = request.Log.content
	{id,email,question} = content


// tasks performed by someone in the front office

determineProcedure :: (Log ClientRequest) -> Task Procedure 							
determineProcedure request
	=					modify "Client Request:" request "Select Procedure to Follow:" defaultValue				// step 2, select procedure to follow																									// step 1
 	>>= \procedure -> 	getClient id																			// search for client with give id in database
 	>>= \mbClient  ->   if (isNothing mbClient)																	// is client administrated ? 
 						   (   inform foEmail email "Unknown Client Id "  request								// step 4.1, client does not exist, email client
 						   >>| return Stop																		// done
 						   )																		
 						   (checkAccount (fromJust mbClient) procedure)											// continue with step 5 and further
where
	checkAccount client procedure
	=					verify "Is the account valid?" ("Account specified: " <+++ account, 
												        "Accounts known: ", client.accounts)					// step 5, manually verify account number
 	 >>= \correct  -> 	if correct 
 						 (return procedure)																		// it is ok, done
 						 (   inform foEmail email "Unknown Account " request									// step 4.2, account does not exist, email client
 						 >>| return Stop																		// done
 						 )					
							   
	content			   = request.Log.content
	{id,email,account} = content


resolveRequest ::  request -> Task option	| iTask request & iTask option & getOptions request option			
resolveRequest request 
	= 						viewInformation "Handle the following request:" [] request							// step 9,  explain what has to be done
							||-
							editChoice "Select Option: " [] (getOptions request) Nothing						// step 10, choose proper option
	>>= \option ->		return option

// tasks performed by someone in the csa office

logServiceRequest ::  request (Shared[(Case,request)]) Case -> Task Case| iTask request	
logServiceRequest request database caseNo
	= 					modify "Log New Case:" request "Assign Case Number:" caseNo								// step 6, assign a case number to this request
	>>= \caseNo	->		log (caseNo,request) database															// step 7, store pair of (casenumber, request) in database
	>>|					return caseNo						

// tasks performed by someone in the mid office

diagnoseCase :: requestCase -> Task Bool | iTask requestCase	 						
diagnoseCase  requestCase
	=					verify "Can case be handled by the front-office?" requestCase							// step 8, judge how to further handle the procedure

modifyAccount :: procedure ClientNo String -> Task () | iTask procedure & makeModification procedure
modifyAccount procedure cid option 
	= 					(viewInformation "Request made" [] procedure
						-&&-
						viewInformation ("Client " <+++ cid <+++ " will be modified: " <+++ option) [] ())
	>>|					makeModification procedure cid option

// ********************************************************************************************************************************************

// General utility functions

create :: from_user for_user String a -> Task (Log a) | iTask a & toString from_user & toString for_user		// Show form to fill in and add log information
create from_user for_user about form
	= 				updateInformation about [] form
	>>= \content ->	get currentDate 
	>>= \date	 -> return { about = about, received_from = from_name, intended_for = for_name, date =  date, content = content}	
where
	from_name 	= toString (from_user)
	for_name	= toString (for_user)
	
		
modify :: String a String b -> Task b | iTask a & iTask b
modify inputInfo input outputInfo output
	=				(viewInformation inputInfo [] input 							// tell what has to be done, show input received
	  				||-
	  				updateInformation outputInfo [] output)							// and fill in the response the context needs

verify :: String a -> Task Bool | iTask a
verify prompt info 
	= 	viewInformation prompt [] info												// show the information to judge
	>>* [ OnAction ActionYes (always (return True))									// press "Yes" to confirm
		, OnAction ActionNo  (always (return False))								// and   "No"  to decline
		]

log :: a (Shared [a])  -> Task () | iTask a
log value sharedDb
	=		 		viewInformation "Information that will be stored:" [] value		// show information that will be stored
	>>|				upd storeLog sharedDb											// store document in database
	>>|				viewInformation "Information has been stored" [] ()				// inform that information has been stored
where
	storeLog db = [value:db]
	
open :: key (Shared [(key,value)]) -> Task (Maybe (key,value))	| iTask key & iTask value	// search in databse for item with a certain key
open index sharedDb
	=				viewInformation "Retrieving from database:" [] index			// inform that database will be accessed
	>>|				get sharedDb													// read from database
	>>= \content -> case documentRecords content of
						[] 		-> 		viewInformation "Information could not be found" [] ()
									>>| return Nothing
						[found] -> 		viewInformation "Found:" [] found
									>>|	return (Just found)
  where
  documentRecords content = [(idx,doc) \\ (idx,doc) <- content | idx === index ]

inform :: EmailAddr EmailAddr String info -> Task (Log Note) | iTask info							// create an email to inform and send it off...
inform fromName toName subject info 
	= 				get currentDate
	>>= \date ->	updateInformation "Compose an email:" []
						{ about	= subject, received_from = toString fromName, intended_for = toString toName
						, date	= date,    content		 = Note (toMultiLineText info)
						}
	 >>= \mail ->	sendAnEmail	mail
	 >>| 			return mail 
where
	sendAnEmail doc=:{received_from,about,content=Note content} 
		= sendEmail [] received_from toName about content
	
