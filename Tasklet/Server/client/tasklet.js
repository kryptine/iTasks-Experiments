// Instance ID -> Tasklet map
tasklets = {}

function __SaplHtml_handleJSEvent(expr,iid,event){
	
	var tasklet = tasklets[iid];
	var state = tasklet.st;
	
	// Returns a tuple of the JS document and HtmlEventResult	
	// Looks like: [0, "Tuple2", document, HtmlEventResult]	
	var ys = Sapl.feval([expr,[state,iid,event,document]]);
	
	// The result is only in HNF, so both part of the tuple must be forced,
	// but the document can be dropped after that.
	Sapl.feval(ys[2]);
	
	var newstate = Sapl.feval(ys[3]);
	tasklets[iid].st = newstate;
						 
	// toJS to make the result hyperstrict
	var newres = Sapl.toJS(Sapl.feval([tasklet.resultFunc,[newstate]]));
	
	// Send result to the client if it is changed only
	if(!geq(tasklet.lastResult, newres)){
		
		tasklet.lastResult = newres;
		__SaplHtml_fireEvent(document,iid,"result",newres);
	}
}

function __SaplHtml_handleInterfaceCall(expr,iid,args){
	var tasklet = tasklets[iid];
	var state = tasklet.st;
	
	var dynArgs = __Maybe_Nothing();
	
	if(args && args.length && args.length > 0){
		if(args.length == 1){
			dynArgs = __Maybe_Just(toDynamic(args[0]));
		}else{
			dynArgs = __Maybe_Just(toDynamic(Sapl.toTuple(args)));
		}
	}
	
	// Returns a tuple of the JS document, state and an arbitrary value	
	// Looks like: [0, "Tuple3", document, state, res]	
	var ys = Sapl.feval([expr,[state,dynArgs,document]]);
	
	// The result is only in HNF, so both part of the tuple must be forced,
	// but the document can be dropped after that.
	Sapl.feval(ys[2]);	
	
	var newstate = Sapl.feval(ys[3]);
	tasklets[iid].st = newstate;
	
	return Sapl.toJS(ys[4]);
}

function __SaplHtml_fireEvent(d,iid,eventType,eventValue){
	var d = Sapl.feval(d);
	var iid = Sapl.feval(iid);
	var eventType = Sapl.feval(eventType);
	var eventValue = Sapl.heval(eventValue);
	
	tasklets[iid].fire(eventType, eventValue);
	return d;
}

// Creating a closure
function eventHandler(expr){
		
	var h = function(event){
		eval("var tmp = eval(" + expr + ");");
		tmp[1].push(event);
		Sapl.feval(tmp);
	};
		
	return h;
}

// Creating a closure
function interfaceWrapper(expr){
		
	var h = function(args){
		eval("var tmp = eval(" + expr + ");");
		tmp[1].push(arguments);
		return Sapl.feval(tmp);
	};
		
	return h;
}

function loadTasklet(url, cont){

	var xmlhttp=new XMLHttpRequest();

	xmlhttp.onreadystatechange=function()
	{
		if (xmlhttp.readyState==4 && xmlhttp.status==200)
		{
			var response = JSON.parse(xmlhttp.responseText);
			cont(findTasklet(response.content));
		}
	}

	xmlhttp.open("GET",url,true);
	xmlhttp.send();
}

function findTasklet(tui){
	switch (tui.xtype) {
		case "itwc_panel":

			for (var i = 0; i < tui.items.length; i++) {
				var tasklet = findTasklet(tui.items[i]);
				
				if(tasklet) 
					return tasklet;
			}
			return null;
		
		case "itwc_tasklet":
		
			var t = new Tasklet(tui);
			tasklets[tui.taskId] = t;
			return t;
	}
}

/*
 * state: st, resultFunc, lastResult, content
 */

function Tasklet(tui){

	this._listeners = {};
	this.tui = tui;
	
	// evaluate scripts
	if(tui.script != null && tui.script != "" && !sapldebug){
		evalScript(tui.script);
	}		

	eval("var tmp = eval(" + tui.st + ");");
	this.st = tmp;
	tasklets[tui.iid] = this;			

	if(tui.resultFunc != null){
		eval("var tmp = eval(" + tui.resultFunc + ");");
		this.resultFunc = tmp;
		this.lastResult = Sapl.toJS(Sapl.feval([this.resultFunc,[this.st]]));
	}
	
	// create initial content
	this.content = document.createElement('div');
	this.content.innerHTML = tui.html;
	
	this.createInterface();
}

// http://www.nczonline.net/blog/2010/03/09/custom-events-in-javascript/
Tasklet.prototype  = {

    constructor: Tasklet,

	display: function(place){
		place.appendChild(this.content);
		this.attachEventHandlers();
	},

	attachEventHandlers: function(tui){
		
		var events = this.tui.events;
		
		for (var i=0; i<events.length; ++i){
			var elname = events[i][0];
			var eventName = events[i][1];
			var expr = events[i][2];
							
			if(elname == "tasklet"){
				if(eventName == "init"){
					(eventHandler(expr))(this);
				}
			}else{
				var el = document.getElementById(elname);
				el.addEventListener(eventName, eventHandler(expr), false); 
			}
		}	
	},
	
	createInterface: function(){
	
		var is = this.tui.interfaceFuncs;
		this.intf = {};
		
		for (var i=0; i<is.length; ++i){
			var fn = is[i][0];
			var expr = is[i][1];
			
			this.intf[fn] = interfaceWrapper(expr);			
		}
	
	},
	
    addListener: function(type, listener){
        if (typeof this._listeners[type] == "undefined"){
            this._listeners[type] = [];
        }

        this._listeners[type].push(listener);
    },

    fire: function(eventType, eventValue){
		event = { type: eventType };
		if(eventValue) event.value = eventValue;
        event.target = this;

        if (this._listeners[event.type] instanceof Array){
            var listeners = this._listeners[event.type];
            for (var i=0, len=listeners.length; i < len; i++){
                listeners[i].call(this, event);
            }
        }
    },

    removeListener: function(type, listener){
        if (this._listeners[type] instanceof Array){
            var listeners = this._listeners[type];
            for (var i=0, len=listeners.length; i < len; i++){
                if (listeners[i] === listener){
                    listeners.splice(i, 1);
                    break;
                }
            }
        }
    }
};

